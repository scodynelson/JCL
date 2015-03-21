/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import java.io.Serializable;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameDirectory} is the object representation of the 'directory' element of a Lisp 'pathname' type.
 */
public final class PathnameDirectory implements Serializable {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -5895728509152895364L;

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
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).toString();
	}
}
