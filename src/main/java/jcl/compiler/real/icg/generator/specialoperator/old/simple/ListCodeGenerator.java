package jcl.compiler.real.icg.generator.specialoperator.old.simple;

import jcl.LispStruct;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.icg.generator.specialoperator.FunctionCallCodeGenerator;
import jcl.compiler.real.icg.generator.specialoperator.old.special.MacroLambdaCodeGenerator;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ListCodeGenerator implements CodeGenerator<ListStruct> {

	private static final Logger LOGGER = LoggerFactory.getLogger(ListCodeGenerator.class);

	//	@Autowired
	private SymbolFunctionCodeGenerator symbolFunctionCodeGenerator;

	//	@Autowired
	private FunctionCallCodeGenerator functionCallCodeGenerator;

	//	@Autowired
	private MacroLambdaCodeGenerator macroLambdaCodeGenerator;

	//	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final ListStruct input, final JavaClassBuilder classBuilder) {

		final LispStruct firstElement = input.getFirst();
		if (firstElement instanceof SymbolStruct) {
			// generally an application (foobar ...)
			symbolFunctionCodeGenerator.generate((SymbolStruct<?>) firstElement, classBuilder);

			final boolean acceptsMultipleValues = classBuilder.isAcceptsMultipleValues();
			try {
				final boolean firstElementFuncall = CommonLispSymbols.FUNCALL.equals(firstElement);
				final boolean firstElementApply = CommonLispSymbols.APPLY.equals(firstElement);
				classBuilder.setAcceptsMultipleValues(firstElementFuncall || firstElementApply);
//					functionCallCodeGenerator.generate(input, classBuilder); TODO
			} finally {
				classBuilder.setAcceptsMultipleValues(acceptsMultipleValues);
			}
		} else if (firstElement instanceof ListStruct) {
			final ListStruct first = (ListStruct) firstElement;
			final LispStruct firstOfFirst = first.getFirst();

			if (firstOfFirst instanceof SymbolStruct) {
				if (firstOfFirst.equals(SpecialOperatorStruct.LAMBDA_MARKER)) {
//					lambdaCodeGenerator.generate(input, codeGenerator, classBuilder); TODO
				} else if (firstOfFirst.equals(SpecialOperatorStruct.MACRO_MARKER)) {
					macroLambdaCodeGenerator.generate(input, classBuilder);
				} else {
					LOGGER.info("It's something else, {}", first);
				}
			} else {
				generate(first, classBuilder);

				final boolean acceptsMultipleValues = classBuilder.isAcceptsMultipleValues();
				try {
					classBuilder.setAcceptsMultipleValues(false);
//					functionCallCodeGenerator.generate(input, classBuilder); TODO
				} finally {
					classBuilder.setAcceptsMultipleValues(acceptsMultipleValues);
				}
			}
		}
	}
}
