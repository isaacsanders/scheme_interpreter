FILES=env.ss interpreter.ss parser.ss main.ss lexical-address.ss syntax-expand.ss functional-utils.ss

scheme_interpreter.zip: ${FILES}
	zip scheme_interpreter.zip ${FILES}
