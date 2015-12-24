package jcl.compiler.sa.analyzer.defstruct;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.analyzer.LispFormValueValidator;
import jcl.compiler.struct.specialoperator.defstruct.DefstructStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.printer.Printer;
import jcl.structures.StructureClassStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class DefstructExpander extends MacroFunctionExpander<LispStruct> {

	private static final long serialVersionUID = 5336983779662053736L;

	@Autowired
	private LispFormValueValidator validator;

	@Autowired
	private Printer printer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.DEFSTRUCT;
	}

	@Override
	public DefstructStruct expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSize(form, 5, "%DEFSTRUCT");

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		final SymbolStruct structureSymbol = validator.validateObjectType(second, "%DEFSTRUCT", "STRUCTURE NAME", SymbolStruct.class);

		final ListStruct formRestRest = formRest.getRest();

		final LispStruct third = formRestRest.getFirst();
		final SymbolStruct includeStructureSymbol = validator.validateSymbolOrNIL(third, "%DEFSTRUCT", "INCLUDE STRUCTURE NAME");

		StructureClassStruct includeStructureClass = null;
		if (includeStructureSymbol != null) {
			includeStructureClass = includeStructureSymbol.getStructureClass();
			if (includeStructureClass == null) {
				final String printedObject = printer.print(third);
				throw new ProgramErrorException("%DEFSTRUCT: Include structure name '" + printedObject + "' must have an already defined structure class.");
			}
		}

		final ListStruct formRestRestRest = formRestRest.getRest();

		final LispStruct fourth = formRestRestRest.getFirst();
		final SymbolStruct defaultConstructorSymbol = validator.validateSymbolOrNIL(fourth, "%DEFSTRUCT", "DEFAULT CONSTRUCTOR NAME");

		final ListStruct formRestRestRestRest = formRestRestRest.getRest();

		final LispStruct fifth = formRestRestRestRest.getFirst();
		final SymbolStruct printerSymbol = validator.validateSymbolOrNIL(fifth, "%DEFSTRUCT", "PRINTER NAME");

		final ListStruct formRestRestRestRestRest = formRestRestRestRest.getRest();

		final List<SymbolStruct> slots = new ArrayList<>();

		final List<LispStruct> slotArguments = formRestRestRestRestRest.getAsJavaList();
		for (final LispStruct slotArgument : slotArguments) {
			final SymbolStruct slotSymbol = validator.validateObjectType(slotArgument, "%DEFSTRUCT", "STRUCTURE SLOT NAME", SymbolStruct.class);
			slots.add(slotSymbol);
		}

		return new DefstructStruct(structureSymbol, includeStructureClass, defaultConstructorSymbol, printerSymbol, slots);
	}
}
