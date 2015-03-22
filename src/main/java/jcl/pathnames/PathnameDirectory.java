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
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(directoryComponent)
		                            .append(componentType)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final PathnameDirectory rhs = (PathnameDirectory) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(directoryComponent, rhs.directoryComponent)
		                          .append(componentType, rhs.componentType)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(directoryComponent)
		                                                                .append(componentType)
		                                                                .toString();
	}
}
