package jcl.compiler.real.sa.specialoperator.compiler;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.specialoperator.LoadTimeValueAnalyzer;
import jcl.compiler.real.sa.specialoperator.QuoteAnalyzer;
import jcl.structs.lists.ConsStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.symbols.SpecialOperator;

public class StaticFieldAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final StaticFieldAnalyzer INSTANCE = new StaticFieldAnalyzer();

	@Override
	public LispStruct analyze(ListStruct input) {
		// 2 args -  the form to eval at class initialization and the name of the field
		input = input.getRest();
		final String fldName = input.getRest().getFirst().toString();
		final LispStruct form = input.getFirst();
		if (form instanceof ListStruct) {
			ListStruct listForm = (ListStruct) form;
			if (listForm.getFirst().equals(SpecialOperator.QUOTE)) {
				((ConsStruct) input).setCdr(QuoteAnalyzer.INSTANCE.analyze(listForm, fldName));
			} else if (!listForm.getFirst().equals(SpecialOperator.LOAD_TIME_VALUE)) {
				listForm = ListStruct.buildProperList(SpecialOperator.LOAD_TIME_VALUE, form);
				((ConsStruct) input).setCdr(LoadTimeValueAnalyzer.INSTANCE.analyze(listForm, fldName));
			} else {
				((ConsStruct) input).setCdr(LoadTimeValueAnalyzer.INSTANCE.analyze(listForm, fldName));
			}
		} else {
			final ListStruct listForm = ListStruct.buildProperList(SpecialOperator.LOAD_TIME_VALUE, form);
			((ConsStruct) input).setCdr(LoadTimeValueAnalyzer.INSTANCE.analyze(listForm, fldName));
		}
		return NullStruct.INSTANCE; // we've done what we need to do, the form needs to be dropped
	}
}
