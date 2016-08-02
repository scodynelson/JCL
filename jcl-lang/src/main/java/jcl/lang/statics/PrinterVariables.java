/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.BooleanStruct;
import jcl.lang.KeywordStruct;
import jcl.lang.TStruct;
import jcl.lang.VariableStruct;
import jcl.lang.list.NILStruct;
import jcl.lang.number.IntegerStructImpl;

public interface PrinterVariables {

	VariableStruct<BooleanStruct> PRINT_ARRAY = VariableStruct.valueOf("*PRINT-ARRAY*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
	VariableStruct<IntegerStructImpl> PRINT_BASE = new RadixVariable("*PRINT-BASE*", GlobalPackageStruct.COMMON_LISP);
	VariableStruct<BooleanStruct> PRINT_RADIX = VariableStruct.valueOf("*PRINT-RADIX*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<KeywordStruct> PRINT_CASE = PrintCaseVariable.INSTANCE;
	VariableStruct<BooleanStruct> PRINT_CIRCLE = VariableStruct.valueOf("*PRINT-CIRCLE*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<BooleanStruct> PRINT_ESCAPE = VariableStruct.valueOf("*PRINT-ESCAPE*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
	VariableStruct<BooleanStruct> PRINT_GENSYM = VariableStruct.valueOf("*PRINT-GENSYM*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
	VariableStruct<IntegerStructImpl> PRINT_LEVEL = new NonNegNilVariable("*PRINT-LEVEL*", GlobalPackageStruct.COMMON_LISP);
	VariableStruct<IntegerStructImpl> PRINT_LENGTH = new NonNegNilVariable("*PRINT-LENGTH*", GlobalPackageStruct.COMMON_LISP);
	VariableStruct<IntegerStructImpl> PRINT_LINES = new NonNegNilVariable("*PRINT-LINES*", GlobalPackageStruct.COMMON_LISP);
	VariableStruct<IntegerStructImpl> PRINT_MISER_WIDTH = new NonNegNilVariable("*PRINT-MISER-WIDTH*", GlobalPackageStruct.COMMON_LISP);
	VariableStruct<?> PRINT_PPRINT_DISPATCH = VariableStruct.valueOf("*PRINT-PPRINT-DISPATCH*", GlobalPackageStruct.COMMON_LISP, null);
	VariableStruct<BooleanStruct> PRINT_PRETTY = VariableStruct.valueOf("*PRINT-PRETTY*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<BooleanStruct> PRINT_READABLY = VariableStruct.valueOf("*PRINT-READABLY*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStruct<IntegerStructImpl> PRINT_RIGHT_MARGIN = new NonNegNilVariable("*PRINT-RIGHT-MARGIN*", GlobalPackageStruct.COMMON_LISP);
}
