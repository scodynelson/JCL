package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.BlockStruct;
import jcl.lang.LispStruct;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.list.ListStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class BlockExpander extends MacroFunctionExpander<BlockStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.BLOCK;
	}

	@Override
	public BlockStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // BLOCK SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("BLOCK: Incorrect number of arguments: 0. Expected at least 1 argument.");
		}
		final LispStruct first = iterator.next();

		if (!(first instanceof SymbolStruct)) {
			throw new TypeErrorException("BLOCK: NAME must be a Symbol. Got: " + first);
		}
		final SymbolStruct name = (SymbolStruct) first;
		environment.getBlockStack().push(name);

		try {
			final List<LispStruct> forms = new ArrayList<>();
			iterator.forEachRemaining(element -> {
				final LispStruct analyzedElement = formAnalyzer.analyze(element, environment);
				forms.add(analyzedElement);
			});
			return new BlockStruct(name, forms);
		} finally {
			environment.getBlockStack().pop();
		}
	}
}
