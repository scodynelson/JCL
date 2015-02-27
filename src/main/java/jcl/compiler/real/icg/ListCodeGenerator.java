package jcl.compiler.real.icg;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SpecialOperatorElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.icg.specialoperator.FletCodeGenerator;
import jcl.compiler.real.icg.specialoperator.LabelsCodeGenerator;
import jcl.compiler.real.icg.specialoperator.LetCodeGenerator;
import jcl.compiler.real.icg.specialoperator.MacroletCodeGenerator;
import jcl.compiler.real.icg.specialoperator.special.LambdaCodeGenerator;
import jcl.compiler.real.icg.specialoperator.special.MacroLambdaCodeGenerator;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import jcl.system.EnhancedLinkedList;
import org.objectweb.asm.Label;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ListCodeGenerator implements CodeGenerator<ConsElement> {

	private static final Logger LOGGER = LoggerFactory.getLogger(ListCodeGenerator.class);

	public static final ListCodeGenerator INSTANCE = new ListCodeGenerator();

	@Override
	public void generate(final ConsElement input, final IntermediateCodeGenerator codeGenerator) {

		final SimpleElement firstElement = input.getElements().getFirst();
		if (firstElement instanceof SymbolElement) {
			// generally an application (foobar ...)
			if (firstElement instanceof SpecialOperatorElement) {
				SpecialFormCodeGenerator.INSTANCE.generate(input, codeGenerator);
//			} else if (firstElement instanceof Declaration) { // TODO fix??
////                genCodeDeclare(list);
			} else if (formOptimizable(input)) {
				genOptimizedForm(input, codeGenerator);
			} else {
				SymbolFunctionCodeGenerator.INSTANCE.generate((SymbolElement) firstElement, codeGenerator);

				final boolean acceptsMultipleValues = FunctionCallCodeGenerator.INSTANCE.isAcceptsMultipleValues();
				try {
					FunctionCallCodeGenerator.INSTANCE.setAcceptsMultipleValues(
							firstElement.equals(GlobalPackageStruct.COMMON_LISP.intern("FUNCALL").getSymbolStruct())
									|| firstElement.equals(GlobalPackageStruct.COMMON_LISP.intern("APPLY").getSymbolStruct()));
					FunctionCallCodeGenerator.INSTANCE.generate(input, codeGenerator);
				} finally {
					FunctionCallCodeGenerator.INSTANCE.setAcceptsMultipleValues(acceptsMultipleValues);
				}
			}
		} else if (firstElement instanceof ConsElement) {
			final ConsElement first = (ConsElement) firstElement;
			final EnhancedLinkedList<SimpleElement> maybeLast = input.getElements().getAllButFirst();
			// could be ((%lambda bindings...) body) or
			// could be (((%lambda bindings...) body) ...args...)
			if (first.getElements().getFirst() instanceof SymbolStruct) {
				// it's ((%lambda bindings...) body)
				if (first.getElements().getFirst().equals(SpecialOperator.LAMBDA_MARKER)) {
					LambdaCodeGenerator.INSTANCE.generate(input, codeGenerator);
				} else if (first.getElements().getFirst().equals(SpecialOperator.MACRO_MARKER)) {
					MacroLambdaCodeGenerator.INSTANCE.generate(input, codeGenerator);
				} else if (first.getElements().getFirst().equals(SpecialOperator.LET)) {
					LetCodeGenerator.INSTANCE.generate(input, codeGenerator);
				} else if (first.getElements().getFirst().equals(SpecialOperator.FLET)) {
					FletCodeGenerator.INSTANCE.generate(input, codeGenerator);
				} else if (first.getElements().getFirst().equals(SpecialOperator.LABELS)) {
					LabelsCodeGenerator.INSTANCE.generate(input, codeGenerator);
				} else if (first.getElements().getFirst().equals(SpecialOperator.MACROLET)) {
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

	private static boolean formOptimizable(final ConsElement list) {
		return list.getElements().getFirst().equals(GlobalPackageStruct.COMMON_LISP.intern("EQ").getSymbolStruct());
	}

	private static void genOptimizedForm(final ConsElement list, final IntermediateCodeGenerator codeGenerator) {
		final SymbolStruct<?> sym = (SymbolStruct) list.getElements().getFirst();
		if (sym.equals(GlobalPackageStruct.COMMON_LISP.intern("EQ").getSymbolStruct())) {
			final EnhancedLinkedList<SimpleElement> args = list.getElements().getAllButFirst();
			// gen the 2 arguments and leave their values on the stack
			codeGenerator.icgMainLoop(args.getFirst());
			codeGenerator.icgMainLoop(args.getAllButFirst().getFirst());
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
