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
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.SymbolStruct;

public class SpecialFormCodeGenerator {

	public static void genCodeSpecialForm(final IntermediateCodeGenerator icg, final ListStruct list) {
		if (list.getFirst() instanceof SymbolStruct) {
			final SymbolStruct<?> symName = (SymbolStruct) list.getFirst();

			// Determine the special form ) generate its code.
			if (symName.equals(SpecialOperator.BLOCK)) {
				BlockCodeGenerator.genCodeBlock(icg, list);
			} else if (symName.equals(SpecialOperator.CATCH)) {
				CatchCodeGenerator.genCodeCatch(icg, list);
			} else if (symName.equals(SpecialOperator.DECLARE)) {
//                genCodeDeclare(icg, list);
			} else if (symName.equals(SpecialOperator.DEFSTRUCT)) {
				DefstructCodeGenerator.genCodeDefstruct(icg, list);
			} else if (symName.equals(SpecialOperator.EVAL_WHEN)) {
				EvalWhenCodeGenerator.genCodeEvalWhen(icg, list);
			} else if (symName.equals(SpecialOperator.FLET)) {
				FletCodeGenerator.genCodeFlet(icg, list);
			} else if (symName.equals(SpecialOperator.FUNCTION)) {
				FunctionCodeGenerator.genCodeFunction(icg, list);
			} else if (symName.equals(SpecialOperator.GO)) {
				GoCodeGenerator.genCodeGo(icg, list);
			} else if (symName.equals(SpecialOperator.IF)) {
				IfCodeGenerator.genCodeIf(icg, list);
			} else if (symName.equals(SpecialOperator.LAMBDA)) {
				LambdaCodeGenerator.genCodeLambda(icg, list);
			} else if (symName.equals(SpecialOperator.LABELS)) {
				LabelsCodeGenerator.genCodeLabels(icg, list);
			} else if (symName.equals(SpecialOperator.LOAD_TIME_VALUE)) {
				LoadTimeValueCodeGenerator.genCodeLoadTimeValue(icg, list);
			} else if (symName.equals(SpecialOperator.LOCALLY)) {
				LocallyCodeGenerator.genCodeLocally(icg, list);
			} else if (symName.equals(SpecialOperator.MACROLET)) {
				MacroletCodeGenerator.genCodeMacrolet(icg, list);
			} else if (symName.equals(SpecialOperator.MULTIPLE_VALUE_CALL)) {
				MultipleValueCallCodeGenerator.genCodeMultipleValueCall(icg, list);
			} else if (symName.equals(SpecialOperator.MULTIPLE_VALUE_PROG1)) {
				MultipleValueProg1CodeGenerator.genCodeMultipleValueProg1(icg, list);
			} else if (symName.equals(SpecialOperator.PROGN)) {
				PrognCodeGenerator.genCodeProgn(icg, list);
			} else if (symName.equals(SpecialOperator.PROGV)) {
				ProgvCodeGenerator.genCodeProgv(icg, list);
			} else if (symName.equals(SpecialOperator.QUOTE)) {
				QuoteCodeGenerator.genCodeQuote(icg, list);
			} else if (symName.equals(SpecialOperator.RETURN_FROM)) {
				ReturnFromCodeGenerator.genCodeReturnFrom(icg, list);
			} else if (symName.equals(SpecialOperator.SETQ)) {
				SetqCodeGenerator.genCodeSetq(icg, list);
			} else if (symName.equals(SpecialOperator.SYMBOL_MACROLET)) {
				SymbolMacroletCodeGenerator.genCodeSymbolMacrolet(icg, list);
			} else if (symName.equals(SpecialOperator.TAGBODY)) {
				TagbodyCodeGenerator.genCodeTagbody(icg, list);
			} else if (symName.equals(SpecialOperator.TAIL_RECURSION)) {
				TailRecursionCodeGenerator.genCodeTailRecursion(icg, list);
			} else if (symName.equals(SpecialOperator.THE)) {
				TheCodeGenerator.genCodeThe(icg, list);
			} else if (symName.equals(SpecialOperator.THROW)) {
				ThrowCodeGenerator.genCodeThrow(icg, list);
			} else if (symName.equals(SpecialOperator.UNWIND_PROTECT)) {
				UnwindProtectCodeGenerator.genCodeUnwindProtect(icg, list);
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
