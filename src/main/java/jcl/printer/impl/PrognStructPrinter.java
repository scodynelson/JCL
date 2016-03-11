package jcl.printer.impl;

import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class PrognStructPrinter implements LispPrinter<PrognStruct> {

	@Autowired
	private Printer printer;

	@Override
	public String print(final PrognStruct object) {
		final List<LispStruct> forms = object.getForms();
		return forms.stream()
		            .map(form -> printer.print(form))
		            .collect(Collectors.joining(" "));
	}
}
