/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.struct;

import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.RadixVariable;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import jcl.symbols.VariableStruct;
import jcl.types.Float;
import jcl.types.SingleFloat;

/**
 * Interface for defining global reader variables for the JCL system.
 */
public interface ReaderVariables {

	/**
	 * A {@link RadixVariable} type variable used to determine in which radix base to read numeric values.
	 */
	VariableStruct<IntegerStruct> READ_BASE = new RadixVariable("*READ-BASE*", GlobalPackageStruct.COMMON_LISP);

	/**
	 * Determine the format in which to read float numeric values.
	 */
	VariableStruct<Float> READ_DEFAULT_FLOAT_FORMAT = new VariableStruct<>("*READ-DEFAULT-FLOAT-FORMAT*", GlobalPackageStruct.COMMON_LISP, SingleFloat.INSTANCE);

	/**
	 * Determines whether or not the read operation should be evaluated where *read-eval* is used.
	 */
	VariableStruct<BooleanStruct> READ_EVAL = new VariableStruct<>("*READ-EVAL*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);

	/**
	 * Determines whether or not the read operation should suppress the read in lisp tokens.
	 */
	VariableStruct<BooleanStruct> READ_SUPPRESS = new VariableStruct<>("*READ-SUPPRESS*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);

	/**
	 * The global {@link ReadtableStruct} denoting the default readtable to be used throughout the JCL system.
	 */
	VariableStruct<ReadtableStruct> READTABLE = new VariableStruct<>("*READTABLE*", GlobalPackageStruct.COMMON_LISP, new ReadtableStruct());
}
