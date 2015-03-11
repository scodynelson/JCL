package jcl.compiler.real.icg;

import jcl.LispStruct;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FunctionCallCodeGenerator;
import jcl.compiler.real.icg.generator.SpecialFormCodeGenerator;
import jcl.compiler.real.icg.generator.SymbolFunctionCodeGenerator;
import jcl.compiler.real.icg.generator.specialoperator.FletCodeGenerator;
import jcl.compiler.real.icg.generator.specialoperator.LabelsCodeGenerator;
import jcl.compiler.real.icg.generator.specialoperator.LetCodeGenerator;
import jcl.compiler.real.icg.generator.specialoperator.MacroletCodeGenerator;
import jcl.compiler.real.icg.generator.specialoperator.special.LambdaCodeGenerator;
import jcl.compiler.real.icg.generator.specialoperator.special.MacroLambdaCodeGenerator;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.Declaration;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ListCodeGenerator implements CodeGenerator<ListStruct> {

	private static final Logger LOGGER = LoggerFactory.getLogger(ListCodeGenerator.class);

	@Autowired
	private SpecialFormCodeGenerator specialFormCodeGenerator;

	@Autowired
	private SymbolFunctionCodeGenerator symbolFunctionCodeGenerator;

	@Autowired
	private FunctionCallCodeGenerator functionCallCodeGenerator;

	@Autowired
	private LambdaCodeGenerator lambdaCodeGenerator;

	@Autowired
	private MacroLambdaCodeGenerator macroLambdaCodeGenerator;

	@Autowired
	private LetCodeGenerator letCodeGenerator;

	@Autowired
	private FletCodeGenerator fletCodeGenerator;

	@Autowired
	private LabelsCodeGenerator labelsCodeGenerator;

	@Autowired
	private MacroletCodeGenerator macroletCodeGenerator;

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {

		final LispStruct firstElement = input.getFirst();
		if (firstElement instanceof SymbolStruct) {
			// generally an application (foobar ...)
			if (firstElement instanceof SpecialOperator) {
				specialFormCodeGenerator.generate(input, codeGenerator, classBuilder);
			} else if (firstElement instanceof Declaration) {
//                genCodeDeclare(list);
			} else if (formOptimizable(input)) {
				genOptimizedForm(input, codeGenerator, classBuilder);
			} else {
				symbolFunctionCodeGenerator.generate((SymbolStruct<?>) firstElement, codeGenerator, classBuilder);

				final boolean acceptsMultipleValues = functionCallCodeGenerator.isAcceptsMultipleValues();
				try {
					functionCallCodeGenerator.setAcceptsMultipleValues(
							firstElement.equals(GlobalPackageStruct.COMMON_LISP.intern("FUNCALL").getSymbol())
									|| firstElement.equals(GlobalPackageStruct.COMMON_LISP.intern("APPLY").getSymbol()));
					functionCallCodeGenerator.generate(input, codeGenerator, classBuilder);
				} finally {
					functionCallCodeGenerator.setAcceptsMultipleValues(acceptsMultipleValues);
				}
			}
		} else if (firstElement instanceof ListStruct) {
			final ListStruct first = (ListStruct) firstElement;
			final ListStruct maybeLast = input.getRest();
			// could be ((%lambda bindings...) body) or
			// could be (((%lambda bindings...) body) ...args...)
			if (first.getFirst() instanceof SymbolStruct) {
				// it's ((%lambda bindings...) body)
				if (first.getFirst().equals(SpecialOperator.LAMBDA_MARKER)) {
					lambdaCodeGenerator.generate(input, codeGenerator, classBuilder);
				} else if (first.getFirst().equals(SpecialOperator.MACRO_MARKER)) {
					macroLambdaCodeGenerator.generate(input, codeGenerator, classBuilder);
				} else if (first.getFirst().equals(SpecialOperator.LET)) {
					letCodeGenerator.generate(input, codeGenerator, classBuilder);
				} else if (first.getFirst().equals(SpecialOperator.FLET)) {
					fletCodeGenerator.generate(input, codeGenerator, classBuilder);
				} else if (first.getFirst().equals(SpecialOperator.LABELS)) {
					labelsCodeGenerator.generate(input, codeGenerator, classBuilder);
				} else if (first.getFirst().equals(SpecialOperator.MACROLET)) {
					macroletCodeGenerator.generate(input, codeGenerator, classBuilder);
				} else {
					LOGGER.info("It's something else, {}", first);
				}
			} else {
				// assume it's (((%lambda bindings...) body) ...args...)
				generate(first, codeGenerator, classBuilder);

				final boolean acceptsMultipleValues = functionCallCodeGenerator.isAcceptsMultipleValues();
				try {
					functionCallCodeGenerator.setAcceptsMultipleValues(false);
					functionCallCodeGenerator.generate(input, codeGenerator, classBuilder);
				} finally {
					functionCallCodeGenerator.setAcceptsMultipleValues(acceptsMultipleValues);
				}
			}
		}
	}

	private static boolean formOptimizable(final ListStruct list) {
		return list.getFirst().equals(GlobalPackageStruct.COMMON_LISP.intern("EQ").getSymbol());
	}

	private static void genOptimizedForm(final ListStruct list, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		final SymbolStruct<?> sym = (SymbolStruct) list.getFirst();
		if (sym.equals(GlobalPackageStruct.COMMON_LISP.intern("EQ").getSymbol())) {
			final ListStruct args = list.getRest();
			// gen the 2 arguments and leave their values on the stack
			codeGenerator.icgMainLoop(args.getFirst(), classBuilder);
			codeGenerator.icgMainLoop(args.getRest().getFirst(), classBuilder);
			// now gen the VM if test
			// just generate direct VM instructions for eq refs
			// get a uniquifier value
			final Label trueLabel = new Label();
			final Label endLabel = new Label();
			classBuilder.getEmitter().emitIf_acmpeq(trueLabel);
			// if not eq, then the value is NIL
			classBuilder.getEmitter().emitGetstatic("lisp/common/type/Boolean", "NIL", "Llisp/common/type/Symbol;");
			classBuilder.getEmitter().emitGoto(endLabel);
			classBuilder.getEmitter().visitMethodLabel(trueLabel);
			classBuilder.getEmitter().emitGetstatic("lisp/common/type/Boolean", "T", "Llisp/common/type/Symbol;");
			classBuilder.getEmitter().visitMethodLabel(endLabel);
		}
	}
}
