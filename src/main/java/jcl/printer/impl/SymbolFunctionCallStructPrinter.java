package jcl.printer.impl;

import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.compiler.struct.specialoperator.SymbolFunctionCallStruct;
import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SymbolFunctionCallStructPrinter implements LispPrinter<SymbolFunctionCallStruct> {

	@Autowired
	private Printer printer;

	@Override
	public String print(final SymbolFunctionCallStruct object) {

		final StringBuilder builder = new StringBuilder("(");

		final SymbolStruct functionSymbol = object.getSymbolCompilerFunction().getFunctionSymbol();
		final String functionSymbolPrinted = printer.print(functionSymbol);
		builder.append(functionSymbolPrinted);
		builder.append(' ');

		final List<LispStruct> arguments = object.getArguments();
		final String printedArguments =
				arguments.stream()
				         .map(argument -> printer.print(argument))
				         .collect(Collectors.joining(" "));
		builder.append(printedArguments);
		builder.append(')');

		return builder.toString();
	}
}
