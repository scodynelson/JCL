/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.pathname;

import jcl.lang.condition.exception.FileErrorException;

/**
 * The {@link PathnameVersion} is the object representation of the 'version' element of a Lisp 'pathname' type.
 */
public final class PathnameVersion {

	/**
	 * The pathname version value.
	 */
	private final Integer version;

	/**
	 * The pathname version component type.
	 */
	private final PathnameVersionComponentType componentType;

	/**
	 * Public constructor.
	 */
	public PathnameVersion() {
		version = null;
		componentType = PathnameVersionComponentType.NEWEST;
	}

	/**
	 * Public constructor.
	 *
	 * @param version
	 * 		the pathname version
	 */
	public PathnameVersion(final Integer version) {
		if ((version == null) || (version < 1)) {
			// TODO: should this take a stream!??!?
			throw new FileErrorException("Version value cannot be null or less than 1.", null);
		}

		this.version = version;
		componentType = PathnameVersionComponentType.NEWEST;
	}

	/**
	 * Public constructor.
	 *
	 * @param componentType
	 * 		pathname version component type
	 */
	public PathnameVersion(final PathnameVersionComponentType componentType) {
		version = null;
		this.componentType = componentType;
	}

	/**
	 * Getter for pathname version {@link #version} property.
	 *
	 * @return pathname version {@link #version} property
	 */
	public Integer getVersion() {
		return version;
	}

	/**
	 * Getter for pathname version {@link #componentType} property.
	 *
	 * @return pathname version {@link #componentType} property
	 */
	public PathnameVersionComponentType getComponentType() {
		return componentType;
	}
}
