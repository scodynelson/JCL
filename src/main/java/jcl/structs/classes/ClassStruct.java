package jcl.structs.classes;

import jcl.LispStruct;
import jcl.LispType;

import java.util.Collections;
import java.util.List;

/**
 * The {@link ClassStruct} is the object representation of a Lisp 'class' type.
 */
public abstract class ClassStruct extends StandardObjectStruct {

	private final LispType type;
	private final List<Class<LispStruct>> directSuperClasses;
	private final List<Class<LispStruct>> subClasses;

	/**
	 * Public constructor.
	 */
	protected ClassStruct() {
		type = jcl.types.Class.INSTANCE;
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
	protected ClassStruct(final LispType type,
	                      final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		this.type = type;
		this.directSuperClasses = (directSuperClasses == null) ? Collections.emptyList() : directSuperClasses;
		this.subClasses = (subClasses == null) ? Collections.emptyList() : subClasses;
	}

	@Override
	public LispType getType() {
		return type;
	}

	/**
	 * The List of direct superclasses for this Class object.
	 *
	 * @return the list of direct superclasses for this class object
	 */
	public List<Class<LispStruct>> getDirectSuperClasses() {
		return directSuperClasses;
	}

	/**
	 * The List of subclasses for this Class object.
	 *
	 * @return the list of subclasses for this class object
	 */
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
