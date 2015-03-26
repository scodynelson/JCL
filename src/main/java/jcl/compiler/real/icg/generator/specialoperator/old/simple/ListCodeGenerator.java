package jcl.compiler.real.icg.generator.specialoperator.old.simple;

import jcl.LispStruct;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.icg.generator.specialoperator.FunctionCallCodeGenerator;
import jcl.compiler.real.icg.generator.specialoperator.old.special.MacroLambdaCodeGenerator;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
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
			if (formOptimizable(input)) {
				genOptimizedForm(input, classBuilder);
			} else {
				symbolFunctionCodeGenerator.generate((SymbolStruct<?>) firstElement, classBuilder);

				final boolean acceptsMultipleValues = classBuilder.isAcceptsMultipleValues();
				try {
					classBuilder.setAcceptsMultipleValues(
							firstElement.equals(GlobalPackageStruct.COMMON_LISP.intern("FUNCALL").getSymbol())
									|| firstElement.equals(GlobalPackageStruct.COMMON_LISP.intern("APPLY").getSymbol()));
//					functionCallCodeGenerator.generate(input, classBuilder);
				} finally {
					classBuilder.setAcceptsMultipleValues(acceptsMultipleValues);
				}
			}
		} else if (firstElement instanceof ListStruct) {
			final ListStruct first = (ListStruct) firstElement;
			final ListStruct maybeLast = input.getRest();
			// could be ((%lambda bindings...) body) or
			// could be (((%lambda bindings...) body) ...args...)
			if (first.getFirst() instanceof SymbolStruct) {
				// it's ((%lambda bindings...) body)
				if (first.getFirst().equals(SpecialOperatorStruct.LAMBDA_MARKER)) {
//					lambdaCodeGenerator.generate(input, codeGenerator, classBuilder); TODO
				} else if (first.getFirst().equals(SpecialOperatorStruct.MACRO_MARKER)) {
					macroLambdaCodeGenerator.generate(input, classBuilder);
				} else {
					LOGGER.info("It's something else, {}", first);
				}
			} else {
				// assume it's (((%lambda bindings...) body) ...args...)
				generate(first, classBuilder);

				final boolean acceptsMultipleValues = classBuilder.isAcceptsMultipleValues();
				try {
					classBuilder.setAcceptsMultipleValues(false);
//					functionCallCodeGenerator.generate(input, classBuilder);
				} finally {
					classBuilder.setAcceptsMultipleValues(acceptsMultipleValues);
				}
			}
		}
	}

	private void genOptimizedForm(final ListStruct list, final JavaClassBuilder classBuilder) {
		final SymbolStruct<?> sym = (SymbolStruct) list.getFirst();
		if (sym.equals(GlobalPackageStruct.COMMON_LISP.intern("EQ").getSymbol())) {
			final ListStruct args = list.getRest();
			// gen the 2 arguments and leave their values on the stack
			formGenerator.generate(args.getFirst(), classBuilder);
			formGenerator.generate(args.getRest().getFirst(), classBuilder);
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

	private static boolean formOptimizable(final ListStruct list) {
		return list.getFirst().equals(GlobalPackageStruct.COMMON_LISP.intern("EQ").getSymbol());
	}
}
