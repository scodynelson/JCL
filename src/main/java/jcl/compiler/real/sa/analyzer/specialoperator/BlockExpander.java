package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.BlockStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class BlockExpander extends MacroFunctionExpander<BlockStruct> {

	private static final long serialVersionUID = -5185467468586381117L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.BLOCK.setMacroFunctionExpander(this);
	}

	@Override
	public BlockStruct expand(final ListStruct form, final Environment environment) {

		final int inputSize = form.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("BLOCK: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final ListStruct inputRest = form.getRest();

		final LispStruct second = inputRest.getFirst();
		if (!(second instanceof SymbolStruct)) {
			throw new ProgramErrorException("BLOCK: Label must be of type SymbolStruct. Got: " + second);
		}

		final SymbolStruct<?> name = (SymbolStruct<?>) second;
		environment.getBlockStack().push(name);

		try {
			final List<LispStruct> forms = inputRest.getRest().getAsJavaList();

			final List<LispStruct> analyzedForms =
					forms.stream()
					     .map(e -> formAnalyzer.analyze(e, environment))
					     .collect(Collectors.toList());

			return new BlockStruct(name, analyzedForms);
		} finally {
			environment.getBlockStack().pop();
		}
	}
}
