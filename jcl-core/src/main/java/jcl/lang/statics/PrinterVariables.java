/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.KeywordStruct;
import jcl.lang.NILStruct;
import jcl.lang.TStruct;
import jcl.lang.internal.VariableStructImpl;

public interface PrinterVariables {

	VariableStructImpl<BooleanStruct> PRINT_ARRAY = VariableStructImpl.valueOf("*PRINT-ARRAY*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
	VariableStructImpl<IntegerStruct> PRINT_BASE = new RadixVariable("*PRINT-BASE*", GlobalPackageStruct.COMMON_LISP);
	VariableStructImpl<BooleanStruct> PRINT_RADIX = VariableStructImpl.valueOf("*PRINT-RADIX*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStructImpl<KeywordStruct> PRINT_CASE = PrintCaseVariable.INSTANCE;
	VariableStructImpl<BooleanStruct> PRINT_CIRCLE = VariableStructImpl.valueOf("*PRINT-CIRCLE*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStructImpl<BooleanStruct> PRINT_ESCAPE = VariableStructImpl.valueOf("*PRINT-ESCAPE*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
	VariableStructImpl<BooleanStruct> PRINT_GENSYM = VariableStructImpl.valueOf("*PRINT-GENSYM*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
	VariableStructImpl<IntegerStruct> PRINT_LEVEL = new NonNegNilVariable("*PRINT-LEVEL*", GlobalPackageStruct.COMMON_LISP);
	VariableStructImpl<IntegerStruct> PRINT_LENGTH = new NonNegNilVariable("*PRINT-LENGTH*", GlobalPackageStruct.COMMON_LISP);
	VariableStructImpl<IntegerStruct> PRINT_LINES = new NonNegNilVariable("*PRINT-LINES*", GlobalPackageStruct.COMMON_LISP);
	VariableStructImpl<IntegerStruct> PRINT_MISER_WIDTH = new NonNegNilVariable("*PRINT-MISER-WIDTH*", GlobalPackageStruct.COMMON_LISP);
	VariableStructImpl<?> PRINT_PPRINT_DISPATCH = VariableStructImpl.valueOf("*PRINT-PPRINT-DISPATCH*", GlobalPackageStruct.COMMON_LISP, null);
	VariableStructImpl<BooleanStruct> PRINT_PRETTY = VariableStructImpl.valueOf("*PRINT-PRETTY*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStructImpl<BooleanStruct> PRINT_READABLY = VariableStructImpl.valueOf("*PRINT-READABLY*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStructImpl<IntegerStruct> PRINT_RIGHT_MARGIN = new NonNegNilVariable("*PRINT-RIGHT-MARGIN*", GlobalPackageStruct.COMMON_LISP);
}
