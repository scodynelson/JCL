package jcl.compiler.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.BlockStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class BlockExpander extends MacroFunctionExpander<BlockStruct> {

	public static final BlockExpander INSTANCE = new BlockExpander();

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.BLOCK;
	}

	@Override
	public BlockStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // BLOCK SYMBOL

		if (!iterator.hasNext()) {
			throw new ProgramErrorException("BLOCK: Incorrect number of arguments: 0. Expected at least 1 argument.");
		}
		final LispStruct first = iterator.next();

		if (!(first instanceof final SymbolStruct name)) {
			throw new TypeErrorException("BLOCK: NAME must be a Symbol. Got: " + first);
		}
		environment.getBlockStack().push(name);

		try {
			final List<LispStruct> forms = new ArrayList<>();
			iterator.forEachRemaining(element -> {
				final LispStruct analyzedElement = FormAnalyzer.analyze(element, environment);
				forms.add(analyzedElement);
			});
			return new BlockStruct(name, forms);
		} finally {
			environment.getBlockStack().pop();
		}
	}
}
