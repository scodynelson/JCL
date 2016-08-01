package jcl.compiler.sa.analyzer.defstruct;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.struct.specialoperator.defstruct.DefstructStruct;
import jcl.lang.LispStruct;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.StructureClassStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.ListStruct;
import jcl.lang.list.NILStruct;
import org.springframework.stereotype.Component;

@Component
public class DefstructExpander extends MacroFunctionExpander<LispStruct> {

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.DEFSTRUCT;
	}

	@Override
	public DefstructStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // %DEFSTRUCT SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("%DEFSTRUCT: Incorrect number of arguments: 0. Expected at least 4 arguments.");
		}
		final LispStruct first = iterator.next();

		if (!(first instanceof SymbolStruct)) {
			throw new TypeErrorException("%DEFSTRUCT: STRUCTURE-NAME must be a Symbol. Got: " + first);
		}
		final SymbolStruct structureSymbol = (SymbolStruct) first;

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("%DEFSTRUCT: Incorrect number of arguments: 1. Expected at least 4 arguments.");
		}
		final LispStruct second = iterator.next();

		final SymbolStruct includeStructureSymbol;
		if (NILStruct.INSTANCE.equals(second)) {
			includeStructureSymbol = null;
		} else if (second instanceof SymbolStruct) {
			includeStructureSymbol = (SymbolStruct) second;
		} else {
			throw new ProgramErrorException("%DEFSTRUCT: INCLUDE-STRUCTURE-NAME must be a Symbol or NIL. Got: " + second);
		}

		StructureClassStruct includeStructureClass = null;
		if (includeStructureSymbol != null) {
			includeStructureClass = includeStructureSymbol.getStructureClass();
			if (includeStructureClass == null) {
				throw new ProgramErrorException("%DEFSTRUCT: Include structure name '" + second + "' must have an already defined structure class.");
			}
		}

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("%DEFSTRUCT: Incorrect number of arguments: 2. Expected at least 4 arguments.");
		}
		final LispStruct third = iterator.next();

		final SymbolStruct defaultConstructorSymbol;
		if (NILStruct.INSTANCE.equals(third)) {
			defaultConstructorSymbol = null;
		} else if (third instanceof SymbolStruct) {
			defaultConstructorSymbol = (SymbolStruct) third;
		} else {
			throw new ProgramErrorException("%DEFSTRUCT: DEFAULT-CONSTRUCTOR-NAME must be a Symbol or NIL. Got: " + third);
		}

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("%DEFSTRUCT: Incorrect number of arguments: 3. Expected at least 4 arguments.");
		}
		final LispStruct fourth = iterator.next();

		final SymbolStruct printerSymbol;
		if (NILStruct.INSTANCE.equals(fourth)) {
			printerSymbol = null;
		} else if (fourth instanceof SymbolStruct) {
			printerSymbol = (SymbolStruct) fourth;
		} else {
			throw new ProgramErrorException("%DEFSTRUCT: PRINTER-NAME must be a Symbol or NIL. Got: " + fourth);
		}

		final List<SymbolStruct> slots = new ArrayList<>();
		iterator.forEachRemaining(element -> {
			if (!(element instanceof SymbolStruct)) {
				throw new TypeErrorException("%DEFSTRUCT: STRUCTURE-SLOT-NAME must be a Symbol. Got: " + element);
			}
			final SymbolStruct slotSymbol = (SymbolStruct) element;
			slots.add(slotSymbol);
		});

		return new DefstructStruct(structureSymbol, includeStructureClass, defaultConstructorSymbol, printerSymbol, slots);
	}
}
