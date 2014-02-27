package jcl.structs.classes;

import jcl.structs.LispStruct;
import jcl.types.LispType;

import java.util.Collections;
import java.util.List;

/**
 * The {@code ClassStruct} is the object representation of a Lisp 'class' type.
 */
public class ClassStruct extends StandardObjectStruct {

	private final LispType type;
	private final List<Class<LispStruct>> directSuperClasses;
	private final List<Class<LispStruct>> subClasses;

	/**
	 * Public constructor.
	 */
	public ClassStruct() {
		type = jcl.types.classes.Class.INSTANCE;
		directSuperClasses = null;
		subClasses = null;
	}

	/**
	 * Public constructor.
	 *
	 * @param type               the type of the class object
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	public ClassStruct(final LispType type,
					   final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		this.type = type;
		if (directSuperClasses == null) {
			this.directSuperClasses = Collections.emptyList();
		} else {
			this.directSuperClasses = directSuperClasses;
		}
		if (subClasses == null) {
			this.subClasses = Collections.emptyList();
		} else {
			this.subClasses = subClasses;
		}
	}

	@Override
	public LispType getType() {
		return type;
	}

	public List<Class<LispStruct>> getDirectSuperClasses() {
		return directSuperClasses;
	}

	public List<Class<LispStruct>> getSubClasses() {
		return subClasses;
	}

	@Override
	public String toString() {
		return "ClassStruct{"
				+ "type=" + type
				+ ", directSuperClasses=" + directSuperClasses
				+ ", subClasses=" + subClasses
				+ '}';
	}
}
