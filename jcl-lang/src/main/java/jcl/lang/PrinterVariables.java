/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import jcl.lang.list.NILStruct;
import jcl.lang.number.IntegerStruct;

public interface PrinterVariables {

	VariableStruct<BooleanStruct> PRINT_ARRAY = new VariableStruct<>("*PRINT-ARRAY*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
	VariableStruct<IntegerStruct> PRINT_BASE = new RadixVariable("*PRINT-BASE*", GlobalPackageStruct.COMMON_LISP);
	VariableStruct<BooleanStruct> PRINT_RADIX = new VariableStruct<>("*PRINT-RADIX*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<KeywordStruct> PRINT_CASE = PrintCaseVariable.INSTANCE;
	VariableStruct<BooleanStruct> PRINT_CIRCLE = new VariableStruct<>("*PRINT-CIRCLE*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<BooleanStruct> PRINT_ESCAPE = new VariableStruct<>("*PRINT-ESCAPE*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
	VariableStruct<BooleanStruct> PRINT_GENSYM = new VariableStruct<>("*PRINT-GENSYM*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
	VariableStruct<IntegerStruct> PRINT_LEVEL = new NonNegNilVariable("*PRINT-LEVEL*", GlobalPackageStruct.COMMON_LISP);
	VariableStruct<IntegerStruct> PRINT_LENGTH = new NonNegNilVariable("*PRINT-LENGTH*", GlobalPackageStruct.COMMON_LISP);
	VariableStruct<IntegerStruct> PRINT_LINES = new NonNegNilVariable("*PRINT-LINES*", GlobalPackageStruct.COMMON_LISP);
	VariableStruct<IntegerStruct> PRINT_MISER_WIDTH = new NonNegNilVariable("*PRINT-MISER-WIDTH*", GlobalPackageStruct.COMMON_LISP);
	VariableStruct<?> PRINT_PPRINT_DISPATCH = new VariableStruct<>("*PRINT-PPRINT-DISPATCH*", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<BooleanStruct> PRINT_PRETTY = new VariableStruct<>("*PRINT-PRETTY*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<BooleanStruct> PRINT_READABLY = new VariableStruct<>("*PRINT-READABLY*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<IntegerStruct> PRINT_RIGHT_MARGIN = new NonNegNilVariable("*PRINT-RIGHT-MARGIN*", GlobalPackageStruct.COMMON_LISP);
}
