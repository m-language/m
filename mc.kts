import java.io.File

fun exit(message: String = "Terminating build") {
    System.err.println(message)
    System.exit(0)
}

fun tryOrExit(fn: () -> Unit) {
    try {
        fn()
    } catch (e: Exception) {
        e.printStackTrace()
        exit()
    }
}

fun exec(string: String, file: File = File(".")) {
    val exec = ProcessBuilder(string.split(' ').toList())
            .redirectErrorStream(true)
            .inheritIO().directory(file.absoluteFile).start()
    val code = exec.waitFor()
    if (code != 0) exit("Command $string failed with exit code $code")
}

fun execM(input: String, string: String, file: File = File(".")) {
    val exec = ProcessBuilder("java -classpath ${bin.absolutePath}${File.pathSeparator}${mJvmJar.absolutePath} -Xss16m mc $input".split(' ').toList())
            .redirectError(ProcessBuilder.Redirect.INHERIT).directory(file.absoluteFile).start()
    exec.outputStream.write("$string\n\n".toByteArray())
    exec.outputStream.flush()
    val code = exec.waitFor()
    if (code != 0) exit("Command $string failed with exit code $code")
}

tailrec fun ask(message: String, yes: () -> Unit) {
    print("$message [y/n]: ")
    System.out.flush()
    val line = readLine()?.toLowerCase()?.trim() ?: "n"
    when (line) {
        "y", "yes" -> yes()
        "n", "no" -> exit()
        else -> {
            println("Expected [y/n]")
            ask(message, yes)
        }
    }
}

val bin = File("./bin")
val mStdlib = File("../m-stdlib")
val mJvm = File("../m-jvm")
val mJvmJar = File(mJvm, "build/libs/m-jvm-0.1.0.jar")

fun help() {
    println("""
        mc.kts help  -- Displays this help message
        mc.kts clean -- Cleans the M compiler
        mc.kts build -- Builds the M compiler
        mc.kts repl  -- Launches the M repl
    """.trimIndent())
}

fun clean() {
    println("Removing bin")
    bin.deleteRecursively()

    println("Cleaning m-jvm")
    exec("gradle clean", mJvm)
}

fun build() {
    if (!mJvm.exists()) {
        val mJvmGit = "https://github.com/m-language/m-jvm.git"
        ask("$mJvm does not exist, would you like to clone it from $mJvmGit?") {
            exec("git clone $mJvmGit", File(".."))
        }
    }

    println("Building m-jvm jar")
    exec("gradle fatJar", mJvm)

    val mJvmCompile = { input: String, output: String ->
        println("Compiling $input with m-jvm")
        exec("java -classpath $mJvmJar -Xss4m io.github.m.Compiler $input $output")
    }

    mJvmCompile("mc.m", bin.path)

    val mCompile = { input: String, output: String ->
        println("Compiling $input with m-compiler")
        exec("java -classpath ./bin${File.pathSeparator}$mJvmJar -Xss16m mc $input $output")
    }

    mCompile("mc.m", bin.path)
    mCompile("mc.m", bin.path)

    println("Compiling m-stdlib")

    if (!mStdlib.exists()) {
        val mStdlibGit = "https://github.com/m-language/m-stdlib.git"
        ask("$mStdlib does not exist, would you like to clone it from $mStdlibGit?") {
            exec("git clone $mStdlibGit", File(".."))
        }
    }

    execM("../m-compiler/mc.m", "!(mpm-put (file.child file.local-file (symbol src)))", mStdlib)

    mCompile("src", bin.path)
    mCompile("src", bin.path)

    println("Regenerating mc.m")
    execM("src", "!(desugar-file (file.child file.local-file (symbol src)) (file.child file.local-file (symbol mc.m)))")
}

fun repl() {
    build()
    val exec = ProcessBuilder("java -classpath ./bin${File.pathSeparator}$mJvmJar -Xss16m mc src".split(' ').toList()).inheritIO().start()
    val code = exec.waitFor()
    if (code != 0) exit("REPL failed with exit code $code")
}

when (args[0]) {
    "help" -> help()
    "clean" -> clean()
    "build" -> build()
    "repl" -> repl()
}