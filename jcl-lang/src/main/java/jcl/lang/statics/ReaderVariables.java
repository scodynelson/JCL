/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.BooleanStruct;
import jcl.lang.readtable.ReadtableStruct;
import jcl.lang.TStruct;
import jcl.lang.VariableStruct;
import jcl.lang.list.NILStruct;
import jcl.lang.number.IntegerStructImpl;
import jcl.type.FloatType;
import jcl.type.SingleFloatType;

/**
 * Interface for defining global reader variables for the JCL system.
 */
public interface ReaderVariables {

	/**
	 * A {@link RadixVariable} type variable used to determine in which radix base to read numeric values.
	 */
	VariableStruct<IntegerStructImpl> READ_BASE = new RadixVariable("*READ-BASE*", GlobalPackageStruct.COMMON_LISP);

	/**
	 * Determine the format in which to read float numeric values.
	 */
	VariableStruct<FloatType> READ_DEFAULT_FLOAT_FORMAT = VariableStruct.valueOf("*READ-DEFAULT-FLOAT-FORMAT*", GlobalPackageStruct.COMMON_LISP, SingleFloatType.INSTANCE);

	/**
	 * Determines whether or not the read operation should be evaluated where *read-eval* is used.
	 */
	VariableStruct<BooleanStruct> READ_EVAL = VariableStruct.valueOf("*READ-EVAL*", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);

	/**
	 * Determines whether or not the read operation should suppress the read in lisp tokens.
	 */
	VariableStruct<BooleanStruct> READ_SUPPRESS = VariableStruct.valueOf("*READ-SUPPRESS*", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);

	/**
	 * The global {@link ReadtableStruct} denoting the default readtable to be used throughout the JCL system.
	 */
	VariableStruct<ReadtableStruct> READTABLE = VariableStruct.valueOf("*READTABLE*", GlobalPackageStruct.COMMON_LISP, ReadtableStruct.valueOf());
}
