package jcl.compiler.real.icg;

import jcl.compiler.real.icg.specialoperator.BlockCodeGenerator;
import jcl.compiler.real.icg.specialoperator.CatchCodeGenerator;
import jcl.compiler.real.icg.specialoperator.EvalWhenCodeGenerator;
import jcl.compiler.real.icg.specialoperator.FletCodeGenerator;
import jcl.compiler.real.icg.specialoperator.FunctionCodeGenerator;
import jcl.compiler.real.icg.specialoperator.GoCodeGenerator;
import jcl.compiler.real.icg.specialoperator.IfCodeGenerator;
import jcl.compiler.real.icg.specialoperator.LabelsCodeGenerator;
import jcl.compiler.real.icg.specialoperator.LoadTimeValueCodeGenerator;
import jcl.compiler.real.icg.specialoperator.LocallyCodeGenerator;
import jcl.compiler.real.icg.specialoperator.MacroletCodeGenerator;
import jcl.compiler.real.icg.specialoperator.MultipleValueCallCodeGenerator;
import jcl.compiler.real.icg.specialoperator.MultipleValueProg1CodeGenerator;
import jcl.compiler.real.icg.specialoperator.PrognCodeGenerator;
import jcl.compiler.real.icg.specialoperator.ProgvCodeGenerator;
import jcl.compiler.real.icg.specialoperator.QuoteCodeGenerator;
import jcl.compiler.real.icg.specialoperator.ReturnFromCodeGenerator;
import jcl.compiler.real.icg.specialoperator.SetqCodeGenerator;
import jcl.compiler.real.icg.specialoperator.SymbolMacroletCodeGenerator;
import jcl.compiler.real.icg.specialoperator.TagbodyCodeGenerator;
import jcl.compiler.real.icg.specialoperator.TheCodeGenerator;
import jcl.compiler.real.icg.specialoperator.ThrowCodeGenerator;
import jcl.compiler.real.icg.specialoperator.UnwindProtectCodeGenerator;
import jcl.compiler.real.icg.specialoperator.compiler.DefstructCodeGenerator;
import jcl.compiler.real.icg.specialoperator.compiler.TailRecursionCodeGenerator;
import jcl.compiler.real.icg.specialoperator.special.LambdaCodeGenerator;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;

public class SpecialFormCodeGenerator implements CodeGenerator<ListStruct> {

	public static final SpecialFormCodeGenerator INSTANCE = new SpecialFormCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		if (input.getFirst() instanceof SymbolStruct) {
			final SymbolStruct<?> symName = (SymbolStruct) input.getFirst();

			// Determine the special form ) generate its code.
			if (symName.equals(SpecialOperator.BLOCK)) {
				BlockCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.CATCH)) {
				CatchCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.DECLARE)) {
//                genCodeDeclare(icg, list);
			} else if (symName.equals(SpecialOperator.DEFSTRUCT)) {
				DefstructCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.EVAL_WHEN)) {
				EvalWhenCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.FLET)) {
				FletCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.FUNCTION)) {
				FunctionCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.GO)) {
				GoCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.IF)) {
				IfCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.LAMBDA)) {
				LambdaCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.LABELS)) {
				LabelsCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.LOAD_TIME_VALUE)) {
				LoadTimeValueCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.LOCALLY)) {
				LocallyCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.MACROLET)) {
				MacroletCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.MULTIPLE_VALUE_CALL)) {
				MultipleValueCallCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.MULTIPLE_VALUE_PROG1)) {
				MultipleValueProg1CodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.PROGN)) {
				PrognCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.PROGV)) {
				ProgvCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.QUOTE)) {
				QuoteCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.RETURN_FROM)) {
				ReturnFromCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.SETQ)) {
				SetqCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.SYMBOL_MACROLET)) {
				SymbolMacroletCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.TAGBODY)) {
				TagbodyCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.TAIL_RECURSION)) {
				TailRecursionCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.THE)) {
				TheCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.THROW)) {
				ThrowCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			} else if (symName.equals(SpecialOperator.UNWIND_PROTECT)) {
				UnwindProtectCodeGenerator.INSTANCE.generate(input, codeGenerator, classBuilder);
			}
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
