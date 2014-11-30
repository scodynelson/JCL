/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer;

import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import jcl.symbols.Variable;

public interface PrinterVariables {

    Variable<BooleanStruct> PRINT_ARRAY = new Variable<>("*PRINT-ARRAY*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
    Variable<IntegerStruct> PRINT_BASE = new RadixVariable("*PRINT-BASE*", GlobalPackageStruct.COMMON_LISP);
    Variable<BooleanStruct> PRINT_RADIX = new Variable<>("*PRINT-RADIX*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
    Variable<KeywordSymbolStruct> PRINT_CASE = PrintCaseVariable.INSTANCE;
    Variable<BooleanStruct> PRINT_CIRCLE = new Variable<>("*PRINT-CIRCLE*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
    Variable<BooleanStruct> PRINT_ESCAPE = new Variable<>("*PRINT-ESCAPE*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
    Variable<BooleanStruct> PRINT_GENSYM = new Variable<>("*PRINT-GENSYM*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
    Variable<IntegerStruct> PRINT_LEVEL = new NonNegNilVariable("*PRINT-LEVEL*", GlobalPackageStruct.COMMON_LISP);
    Variable<IntegerStruct> PRINT_LENGTH = new NonNegNilVariable("*PRINT-LENGTH*", GlobalPackageStruct.COMMON_LISP);
    Variable<IntegerStruct> PRINT_LINES = new NonNegNilVariable("*PRINT-LINES*", GlobalPackageStruct.COMMON_LISP);
    Variable<IntegerStruct> PRINT_MISER_WIDTH = new NonNegNilVariable("*PRINT-MISER-WIDTH*", GlobalPackageStruct.COMMON_LISP);
    Variable<?> PRINT_PPRINT_DISPATCH = new Variable<>("*PRINT-PPRINT-DISPATCH*", GlobalPackageStruct.COMMON_LISP, null);
    Variable<BooleanStruct> PRINT_PRETTY = new Variable<>("*PRINT-PRETTY*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
    Variable<BooleanStruct> PRINT_READABLY = new Variable<>("*PRINT-READABLY*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
    Variable<IntegerStruct> PRINT_RIGHT_MARGIN = new NonNegNilVariable("*PRINT-RIGHT-MARGIN*", GlobalPackageStruct.COMMON_LISP);
}
