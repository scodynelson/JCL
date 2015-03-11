/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg;

public interface IntermediateCodeGenerator {

	Object funcall(Object lispFunc);

	void icgMainLoop(Object obj, boolean allowMultipleValues, final JavaClassBuilder classBuilder);

	void icgMainLoop(Object obj, JavaClassBuilder classBuilder);
}
