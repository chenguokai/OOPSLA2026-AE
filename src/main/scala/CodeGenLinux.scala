package HT.Linux

// this file is used to generate the OS-specific code for Linux

def sectionStringASM(section: String): String = {
  s"""
  |section .${section}:
  |""".stripMargin
}

def sectionStringC(section: String): String = {
  s"""
  |__attribute__((section(".customdata.${section}")))
  |""".stripMargin
}

def sectionTextStringC(section: String): String = {
  s"""
     |__attribute__((section(".customtext.${section},\\"ax\\",@progbits")))
     |""".stripMargin
}

def sectionTextStringCNoAX(section: String): String = {
  s"""
     |__attribute__((section(".customtext.${section}")))
     |""".stripMargin
}
def noinlineC(): String = {
  s"""
  |__attribute__((noinline))""".stripMargin
}

def startThread(funcName: String, pidVarName: String): String = {
  // launch a new thread here
  s"""
     |  if (pthread_create(&${pidVarName}, NULL, ${funcName}, NULL) != 0) {
     |        perror("Failed to create thread");
     |        exit(1);
     |  }
     |""".stripMargin
}

def startProcess(funcName: String, pidVarName: String): String = {
  // launch a new process here
  s"""
     |  ${pidVarName} = fork();
     |  if (${pidVarName} == 0) {
     |    ${funcName}(NULL);
     |    exit(0);
     |  }
     |""".stripMargin
}