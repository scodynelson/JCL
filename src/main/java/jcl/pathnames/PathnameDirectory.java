/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameDirectory} is the object representation of the 'directory' element of a Lisp 'pathname' type.
 */
public final class PathnameDirectory {

	/**
	 * The pathname directory value.
	 */
	private final PathnameDirectoryComponent directoryComponent;

	/**
	 * The pathname directory component type.
	 */
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
	 * Getter for pathname directory {@link #directoryComponent} property.
	 *
	 * @return pathname directory {@link #directoryComponent} property
	 */
	public PathnameDirectoryComponent getDirectoryComponent() {
		return directoryComponent;
	}

	/**
	 * Getter for pathname directory {@link #componentType} property.
	 *
	 * @return pathname directory {@link #componentType} property
	 */
	public PathnameComponentType getComponentType() {
		return componentType;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
