/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import jcl.compiler.GenerationUnsupportedException;
import jcl.compiler.icg.GeneratorState;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link LispStruct} is the representation for all Lisp types.
 */
public interface LispStruct {

	default boolean eq(final LispStruct object) {
		return this == object;
	}

	default boolean eql(final LispStruct object) {
		return eq(object);
	}

	default boolean equal(final LispStruct object) {
		return eq(object);
	}

	default boolean equalp(final LispStruct object) {
		return equal(object);
	}

	default void generate(final GeneratorState generatorState) {
		throw new GenerationUnsupportedException();
	}

	default LispStruct documentation() {
		return NILStruct.INSTANCE;
	}

	default LispStruct setDocumentation(final StringStruct docString) {
		return NILStruct.INSTANCE;
	}

	@Deprecated
	default LispStruct setDocumentation(final String docString) {
		return NILStruct.INSTANCE;
	}

	default LispStruct typeOf() {
		return CommonLispSymbols.T;
	}

	default ClassStruct classOf() {
		return BuiltInClassStruct.CLASS_T;
	}

	default BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.T) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.CLASS_T) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.ATOM) {
			return TStruct.INSTANCE;
		}
		return NILStruct.INSTANCE;
	}
}
