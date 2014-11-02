package jcl.compiler.real.icg;

import jcl.LispStruct;
import jcl.compiler.real.icg.specialoperator.SpecialFormCodeGenerator;
import jcl.structs.lists.ListStruct;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.structs.symbols.Declaration;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ListCodeGenerator {

	private static final Logger LOGGER = LoggerFactory.getLogger(ListCodeGenerator.class);

	public static void genCodeList(final IntermediateCodeGenerator icg, final ListStruct list) {

		final LispStruct firstElement = list.getFirst();
		if (firstElement instanceof SymbolStruct) {
			// generally an application (foobar ...)
			if (firstElement instanceof SpecialOperator) {
				SpecialFormCodeGenerator.genCodeSpecialForm(icg, list);
			} else if (firstElement instanceof Declaration) {
//                genCodeDeclare(list);
			} else if (formOptimizable(list)) {
				genOptimizedForm(icg, list);
			} else {
				SymbolFunctionCodeGenerator.genCodeSymbolFunction(icg, (SymbolStruct) firstElement);
				FunctionCallCodeGenerator.genCodeFunctionCall(icg, list,
						firstElement.equals(GlobalPackageStruct.COMMON_LISP.intern("FUNCALL").getSymbolStruct())
								|| firstElement.equals(GlobalPackageStruct.COMMON_LISP.intern("APPLY").getSymbolStruct()));
			}
		} else if (firstElement instanceof ListStruct) {
			final ListStruct first = (ListStruct) firstElement;
			final ListStruct maybeLast = list.getRest();
			// could be ((%lambda bindings...) body) or
			// could be (((%lambda bindings...) body) ...args...)
			if (first.getFirst() instanceof SymbolStruct) {
				// it's ((%lambda bindings...) body)
				if (first.getFirst().equals(SpecialOperator.LAMBDA_MARKER)) {
					LambdaCodeGenerator.genCodeLambda(icg, list);
				} else if (first.getFirst().equals(SpecialOperator.MACRO_MARKER)) {
					MacroLambdaCodeGenerator.genCodeMacroLambda(icg, list);
				} else if (first.getFirst().equals(SpecialOperator.LET)) {
					LetCodeGenerator.genCodeLet(icg, list);
				} else if (first.getFirst().equals(SpecialOperator.FLET)) {
					FletCodeGenerator.genCodeFlet(icg, list);
				} else if (first.getFirst().equals(SpecialOperator.LABELS)) {
					LabelsCodeGenerator.genCodeLabels(icg, list);
				} else if (first.getFirst().equals(SpecialOperator.MACROLET)) {
					MacroletCodeGenerator.genCodeMacrolet(icg, list);
				} else {
					LOGGER.info("It's something else, {}", first);
				}
			} else {
				// assume it's (((%lambda bindings...) body) ...args...)
				genCodeList(icg, first);
				FunctionCallCodeGenerator.genCodeFunctionCall(icg, list, false);
			}
		}
	}

	private static boolean formOptimizable(final ListStruct list) {
		return list.getFirst().equals(GlobalPackageStruct.COMMON_LISP.intern("EQ").getSymbolStruct());
	}

	private static void genOptimizedForm(final IntermediateCodeGenerator icg, final ListStruct list) {
		final SymbolStruct<?> sym = (SymbolStruct) list.getFirst();
		if (sym.equals(GlobalPackageStruct.COMMON_LISP.intern("EQ").getSymbolStruct())) {
			final ListStruct args = list.getRest();
			// gen the 2 arguments and leave their values on the stack
			icg.icgMainLoop(args.getFirst());
			icg.icgMainLoop(args.getRest().getFirst());
			// now gen the VM if test
			// just generate direct VM instructions for eq refs
			// get a uniquifier value
			final Label trueLabel = new Label();
			final Label endLabel = new Label();
			icg.emitter.emitIf_acmpeq(trueLabel);
			// if not eq, then the value is NIL
			icg.emitter.emitGetstatic("lisp/common/type/Boolean", "NIL", "Llisp/common/type/Symbol;");
			icg.emitter.emitGoto(endLabel);
			icg.emitter.visitMethodLabel(trueLabel);
			icg.emitter.emitGetstatic("lisp/common/type/Boolean", "T", "Llisp/common/type/Symbol;");
			icg.emitter.visitMethodLabel(endLabel);
		}
	}
}
