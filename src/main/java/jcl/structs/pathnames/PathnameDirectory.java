package jcl.structs.pathnames;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameDirectory} is the object representation of the 'directory' element of a Lisp 'pathname' type.
 */
public final class PathnameDirectory {

	private final PathnameDirectoryComponent directoryComponent;
	private final PathnameComponentType componentType;

	/**
	 * Public constructor.
	 */
	public PathnameDirectory() {
		directoryComponent = null;
		componentType = PathnameComponentType.UNSPECIFIC;
	}

	/**
	 * Public constructor.
	 *
	 * @param directoryComponent
	 * 		the pathname directory component
	 */
	public PathnameDirectory(final PathnameDirectoryComponent directoryComponent) {
		this.directoryComponent = directoryComponent;

		if (directoryComponent == null) {
			componentType = PathnameComponentType.NIL;
		} else {
			componentType = null;
		}
	}

	/**
	 * Public constructor.
	 *
	 * @param componentType
	 * 		pathname directory component type
	 */
	public PathnameDirectory(final PathnameComponentType componentType) {
		directoryComponent = null;
		this.componentType = componentType;
	}

	/**
	 * Getter for pathname directory component value.
	 *
	 * @return pathname directory component value
	 */
	public PathnameDirectoryComponent getDirectoryComponent() {
		return directoryComponent;
	}

	/**
	 * Getter for pathname directory component type.
	 *
	 * @return pathname directory component type
	 */
	public PathnameComponentType getComponentType() {
		return componentType;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
