package jcl.lang.classes;

import java.util.Collections;
import java.util.List;

import jcl.lang.SymbolStruct;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link StandardClassStruct} is the object representation of a Lisp 'standard-class' type.
 */
public final class StandardClassStruct extends ClassStruct {

	// TODO: Flush this out more

	public static final StandardClassStruct STANDARD_CLASS =
			addStandardClass(CommonLispSymbols.STANDARD_CLASS, BuiltInClassStruct.CLASS_T);
	public static final StandardClassStruct STANDARD_OBJECT =
			addStandardClass(CommonLispSymbols.STANDARD_OBJECT, BuiltInClassStruct.CLASS_T);
//	public static final StandardClassStruct METAOBJECT =
//			addStandardClass(CommonLispSymbols.METAOBJECT, STANDARD_OBJECT);
//	public static final StandardClassStruct SPECIALIZER =
//			addStandardClass(CommonLispSymbols.SPECIALIZER, METAOBJECT);

	public static final StandardClassStruct CLASS =
			addStandardClass(CommonLispSymbols.CLASS, STANDARD_OBJECT);
//			addStandardClass(CommonLispSymbols.CLASS, SPECIALIZER);

	public static final StandardClassStruct BUILT_IN_CLASS =
			addStandardClass(CommonLispSymbols.BUILT_IN_CLASS, CLASS);

	static {
		STANDARD_CLASS.setDirectSuperClasses(CLASS);
		STANDARD_OBJECT.setDirectSuperClasses(BuiltInClassStruct.CLASS_T);
		BUILT_IN_CLASS.setClassPrecedenceList(BUILT_IN_CLASS, CLASS,
//		                      SPECIALIZER, METAOBJECT,
                                              STANDARD_OBJECT, BuiltInClassStruct.CLASS_T);
		CLASS.setClassPrecedenceList(CLASS,
//		             SPECIALIZER, METAOBJECT,
                                     STANDARD_OBJECT, BuiltInClassStruct.CLASS_T);
		STANDARD_CLASS.setClassPrecedenceList(STANDARD_CLASS, CLASS,
//		                      SPECIALIZER, METAOBJECT,
                                              STANDARD_OBJECT, BuiltInClassStruct.CLASS_T);
		STANDARD_OBJECT.setClassPrecedenceList(STANDARD_OBJECT, BuiltInClassStruct.CLASS_T);
	}

	private StandardClassStruct(final SymbolStruct name, final List<ClassStruct> directSuperClasses) {
		super(name, directSuperClasses);
	}

	private static StandardClassStruct addStandardClass(final SymbolStruct name,
	                                                    final ClassStruct directSuperClass) {
		final StandardClassStruct classStruct = new StandardClassStruct(
				name, Collections.singletonList(directSuperClass)
		);
		addClass(name, classStruct);
		return classStruct;
	}

	private static StandardClassStruct addStandardClass(final SymbolStruct name,
	                                                    final List<ClassStruct> directSuperClasses) {
		final StandardClassStruct classStruct = new StandardClassStruct(name, directSuperClasses);
		addClass(name, classStruct);
		return classStruct;
	}
}
