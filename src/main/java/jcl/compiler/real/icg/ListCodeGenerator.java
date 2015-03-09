package jcl.compiler.real.icg;

import jcl.LispStruct;
import jcl.compiler.real.icg.specialoperator.FletCodeGenerator;
import jcl.compiler.real.icg.specialoperator.LabelsCodeGenerator;
import jcl.compiler.real.icg.specialoperator.LetCodeGenerator;
import jcl.compiler.real.icg.specialoperator.MacroletCodeGenerator;
import jcl.compiler.real.icg.specialoperator.special.LambdaCodeGenerator;
import jcl.compiler.real.icg.specialoperator.special.MacroLambdaCodeGenerator;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.Declaration;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ListCodeGenerator implements CodeGenerator<ListStruct> {

	private static final Logger LOGGER = LoggerFactory.getLogger(ListCodeGenerator.class);

	public static final ListCodeGenerator INSTANCE = new ListCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator) {

		final LispStruct firstElement = input.getFirst();
		if (firstElement instanceof SymbolStruct) {
			// generally an application (foobar ...)
			if (firstElement instanceof SpecialOperator) {
				SpecialFormCodeGenerator.INSTANCE.generate(input, codeGenerator);
			} else if (firstElement instanceof Declaration) {
//                genCodeDeclare(list);
			} else if (formOptimizable(input)) {
				genOptimizedForm(input, codeGenerator);
			} else {
				SymbolFunctionCodeGenerator.INSTANCE.generate((SymbolStruct<?>) firstElement, codeGenerator);

				final boolean acceptsMultipleValues = FunctionCallCodeGenerator.INSTANCE.isAcceptsMultipleValues();
				try {
					FunctionCallCodeGenerator.INSTANCE.setAcceptsMultipleValues(
							firstElement.equals(GlobalPackageStruct.COMMON_LISP.intern("FUNCALL").getSymbol())
									|| firstElement.equals(GlobalPackageStruct.COMMON_LISP.intern("APPLY").getSymbol()));
					FunctionCallCodeGenerator.INSTANCE.generate(input, codeGenerator);
				} finally {
					FunctionCallCodeGenerator.INSTANCE.setAcceptsMultipleValues(acceptsMultipleValues);
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
					LambdaCodeGenerator.INSTANCE.generate(input, codeGenerator);
				} else if (first.getFirst().equals(SpecialOperator.MACRO_MARKER)) {
					MacroLambdaCodeGenerator.INSTANCE.generate(input, codeGenerator);
				} else if (first.getFirst().equals(SpecialOperator.LET)) {
					LetCodeGenerator.INSTANCE.generate(input, codeGenerator);
				} else if (first.getFirst().equals(SpecialOperator.FLET)) {
					FletCodeGenerator.INSTANCE.generate(input, codeGenerator);
				} else if (first.getFirst().equals(SpecialOperator.LABELS)) {
					LabelsCodeGenerator.INSTANCE.generate(input, codeGenerator);
				} else if (first.getFirst().equals(SpecialOperator.MACROLET)) {
					MacroletCodeGenerator.INSTANCE.generate(input, codeGenerator);
				} else {
					LOGGER.info("It's something else, {}", first);
				}
			} else {
				// assume it's (((%lambda bindings...) body) ...args...)
				generate(first, codeGenerator);

				final boolean acceptsMultipleValues = FunctionCallCodeGenerator.INSTANCE.isAcceptsMultipleValues();
				try {
					FunctionCallCodeGenerator.INSTANCE.setAcceptsMultipleValues(false);
					FunctionCallCodeGenerator.INSTANCE.generate(input, codeGenerator);
				} finally {
					FunctionCallCodeGenerator.INSTANCE.setAcceptsMultipleValues(acceptsMultipleValues);
				}
			}
		}
	}

	private static boolean formOptimizable(final ListStruct list) {
		return list.getFirst().equals(GlobalPackageStruct.COMMON_LISP.intern("EQ").getSymbol());
	}

	private static void genOptimizedForm(final ListStruct list, final IntermediateCodeGenerator codeGenerator) {
		final SymbolStruct<?> sym = (SymbolStruct) list.getFirst();
		if (sym.equals(GlobalPackageStruct.COMMON_LISP.intern("EQ").getSymbol())) {
			final ListStruct args = list.getRest();
			// gen the 2 arguments and leave their values on the stack
			codeGenerator.icgMainLoop(args.getFirst());
			codeGenerator.icgMainLoop(args.getRest().getFirst());
			// now gen the VM if test
			// just generate direct VM instructions for eq refs
			// get a uniquifier value
			final Label trueLabel = new Label();
			final Label endLabel = new Label();
			codeGenerator.emitter.emitIf_acmpeq(trueLabel);
			// if not eq, then the value is NIL
			codeGenerator.emitter.emitGetstatic("lisp/common/type/Boolean", "NIL", "Llisp/common/type/Symbol;");
			codeGenerator.emitter.emitGoto(endLabel);
			codeGenerator.emitter.visitMethodLabel(trueLabel);
			codeGenerator.emitter.emitGetstatic("lisp/common/type/Boolean", "T", "Llisp/common/type/Symbol;");
			codeGenerator.emitter.visitMethodLabel(endLabel);
		}
	}
}
