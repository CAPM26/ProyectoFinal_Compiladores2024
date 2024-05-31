
package com.ProyectoFinal_Compiladores2024.ProyectoFinal_Compiladores2024;
import java.io.IOException;

import org.antlr.v4.runtime.ANTLRFileStream;
import org.antlr.v4.runtime.CommonTokenStream;

public class Main {

	private static final String EXTENSION = "pfc";

	public static void main(String[] args) throws IOException {
		String program = args.length > 1 ? args[1] : "test/test." + EXTENSION;

		System.out.println("Interpreting file " + program);

		ProyectoFinal_Compiladores2024Lexer lexer = new ProyectoFinal_Compiladores2024Lexer(new ANTLRFileStream(program));
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		ProyectoFinal_Compiladores2024Parser parser = new ProyectoFinal_Compiladores2024Parser(tokens);

		ProyectoFinal_Compiladores2024Parser.ProgramContext tree = parser.program();

		ProyectoFinal_Compiladores2024CustomVisitor visitor = new ProyectoFinal_Compiladores2024CustomVisitor();
		visitor.visit(tree);

		System.out.println("Interpretation finished");

	}

}
