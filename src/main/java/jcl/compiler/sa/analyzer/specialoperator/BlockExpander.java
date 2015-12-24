package jcl.compiler.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.LispFormValueValidator;
import jcl.compiler.struct.specialoperator.BlockStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class BlockExpander extends MacroFunctionExpander<BlockStruct> {

	private static final long serialVersionUID = -5185467468586381117L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private LispFormValueValidator validator;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.BLOCK;
	}

	@Override
	public BlockStruct expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSize(form, 2, "BLOCK");

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		final SymbolStruct name = validator.validateObjectType(second, "BLOCK", "NAME", SymbolStruct.class);
		environment.getBlockStack().push(name);

		try {
			final ListStruct formRestRest = formRest.getRest();

			final List<LispStruct> forms = formRestRest.getAsJavaList();
			final List<LispStruct> analyzedForms =
					forms.stream()
					     .map(e -> formAnalyzer.analyze(e, environment))
					     .collect(Collectors.toList());

			return new BlockStruct(name, new PrognStruct(analyzedForms));
		} finally {
			environment.getBlockStack().pop();
		}
	}
}
