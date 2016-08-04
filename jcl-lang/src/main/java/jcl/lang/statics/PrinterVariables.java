/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.BooleanStructImpl;
import jcl.lang.IntegerStruct;
import jcl.lang.KeywordStructImpl;
import jcl.lang.TStruct;
import jcl.lang.internal.VariableStructImpl;
import jcl.lang.NILStruct;

public interface PrinterVariables {

	VariableStructImpl<BooleanStructImpl> PRINT_ARRAY = VariableStructImpl.valueOf("*PRINT-ARRAY*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
	VariableStructImpl<IntegerStruct> PRINT_BASE = new RadixVariable("*PRINT-BASE*", GlobalPackageStruct.COMMON_LISP);
	VariableStructImpl<BooleanStructImpl> PRINT_RADIX = VariableStructImpl.valueOf("*PRINT-RADIX*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStructImpl<KeywordStructImpl> PRINT_CASE = PrintCaseVariable.INSTANCE;
	VariableStructImpl<BooleanStructImpl> PRINT_CIRCLE = VariableStructImpl.valueOf("*PRINT-CIRCLE*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStructImpl<BooleanStructImpl> PRINT_ESCAPE = VariableStructImpl.valueOf("*PRINT-ESCAPE*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
	VariableStructImpl<BooleanStructImpl> PRINT_GENSYM = VariableStructImpl.valueOf("*PRINT-GENSYM*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
	VariableStructImpl<IntegerStruct> PRINT_LEVEL = new NonNegNilVariable("*PRINT-LEVEL*", GlobalPackageStruct.COMMON_LISP);
	VariableStructImpl<IntegerStruct> PRINT_LENGTH = new NonNegNilVariable("*PRINT-LENGTH*", GlobalPackageStruct.COMMON_LISP);
	VariableStructImpl<IntegerStruct> PRINT_LINES = new NonNegNilVariable("*PRINT-LINES*", GlobalPackageStruct.COMMON_LISP);
	VariableStructImpl<IntegerStruct> PRINT_MISER_WIDTH = new NonNegNilVariable("*PRINT-MISER-WIDTH*", GlobalPackageStruct.COMMON_LISP);
	VariableStructImpl<?> PRINT_PPRINT_DISPATCH = VariableStructImpl.valueOf("*PRINT-PPRINT-DISPATCH*", GlobalPackageStruct.COMMON_LISP, null);
	VariableStructImpl<BooleanStructImpl> PRINT_PRETTY = VariableStructImpl.valueOf("*PRINT-PRETTY*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStructImpl<BooleanStructImpl> PRINT_READABLY = VariableStructImpl.valueOf("*PRINT-READABLY*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	VariableStructImpl<IntegerStruct> PRINT_RIGHT_MARGIN = new NonNegNilVariable("*PRINT-RIGHT-MARGIN*", GlobalPackageStruct.COMMON_LISP);
}
