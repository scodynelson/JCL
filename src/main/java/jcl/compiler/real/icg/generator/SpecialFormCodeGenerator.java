package jcl.compiler.real.icg.generator;

import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class SpecialFormCodeGenerator implements CodeGenerator<ListStruct> {

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		if (input.getFirst() instanceof SymbolStruct) {
			final SymbolStruct<?> symName = (SymbolStruct) input.getFirst();

			// Determine the special form ) generate its code.
//			if (symName.equals(SpecialOperator.BLOCK)) {
//				BlockCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.CATCH)) {
//				CatchCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.DECLARE)) {
////                genCodeDeclare(icg, list);
//			} else if (symName.equals(SpecialOperator.DEFSTRUCT)) {
//				DefstructCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.EVAL_WHEN)) {
//				EvalWhenCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.FLET)) {
//				FletCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.FUNCTION)) {
//				FunctionCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.GO)) {
//				GoCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.IF)) {
//				IfCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.LAMBDA)) {
//				LambdaCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.LABELS)) {
//				LabelsCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.LOAD_TIME_VALUE)) {
//				LoadTimeValueCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.LOCALLY)) {
//				LocallyCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.MACROLET)) {
//				MacroletCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.MULTIPLE_VALUE_CALL)) {
//				MultipleValueCallCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.MULTIPLE_VALUE_PROG1)) {
//				MultipleValueProg1CodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.PROGN)) {
//				PrognCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.PROGV)) {
//				ProgvCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.QUOTE)) {
//				QuoteCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.RETURN_FROM)) {
//				ReturnFromCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.SETQ)) {
//				SetqCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.SYMBOL_MACROLET)) {
//				SymbolMacroletCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.TAGBODY)) {
//				TagbodyCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.TAIL_RECURSION)) {
//				TailRecursionCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.THE)) {
//				TheCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.THROW)) {
//				ThrowCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			} else if (symName.equals(SpecialOperator.UNWIND_PROTECT)) {
//				UnwindProtectCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
//			}
		} else {
			// handle when the car is a list - ((%lambda ....)... ) or ((%let...) ...)
//            if (icg, list.getCar() instanceof ListStruct) {
//                //get car of the car of the list - (%lambda ...)
//                if (((ListStruct)list.getCar()).getCar() == LAMBDA) {
//                    genCodeLambda(icg, list);
//                    //not done yet
//                }
//            }
		}
	}
}
