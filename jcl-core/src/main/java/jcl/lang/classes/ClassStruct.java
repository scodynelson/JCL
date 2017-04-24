package jcl.lang.classes;

import java.util.Collections;
import java.util.List;

import jcl.lang.LispStruct;
import jcl.type.ClassType;
import jcl.type.LispType;

/**
 * The {@link ClassStruct} is the object representation of a Lisp 'class' type.
 */
public abstract class ClassStruct extends StandardObjectStruct {

	private final LispType type;

	private final List<Class<? extends LispStruct>> directSuperClasses;

	private final List<Class<? extends LispStruct>> subClasses;

	/**
	 * Protected constructor.
	 */
	protected ClassStruct() {
		this(null);
	}

	/**
	 * Protected constructor.
	 *
	 * @param documentation
	 * 		instance documentation string
	 */
	protected ClassStruct(final String documentation) {
		this(documentation, ClassType.INSTANCE, null, null);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the class object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected ClassStruct(final LispType type,
	                      final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(null, type, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param documentation
	 * 		instance documentation string
	 * @param type
	 * 		the type of the class object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected ClassStruct(final String documentation, final LispType type,
	                      final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(documentation);
		this.type = type;
		this.directSuperClasses = (directSuperClasses == null) ? Collections.emptyList() : directSuperClasses;
		this.subClasses = (subClasses == null) ? Collections.emptyList() : subClasses;
	}

	@Override
	public LispType getType() {
		return type;
	}

	/**
	 * Getter for class {@link #directSuperClasses} property.
	 *
	 * @return class {@link #directSuperClasses} property
	 */
	public List<Class<? extends LispStruct>> getDirectSuperClasses() {
		return directSuperClasses;
	}

	/**
	 * Getter for standard object {@link #subClasses} property.
	 *
	 * @return standard object {@link #subClasses} property
	 */
	public List<Class<? extends LispStruct>> getSubClasses() {
		return subClasses;
	}
}
