/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.pathname;

import java.util.Collections;
import java.util.List;

import jcl.lang.condition.exception.FileErrorException;
import org.apache.commons.collections4.CollectionUtils;

/**
 * The {@link PathnameDirectoryComponent} is the object representation of a directory component of the 'directory'
 * element of a Lisp 'pathname' type.
 */
public final class PathnameDirectoryComponent {

	/**
	 * The pathname list of directory level values.
	 */
	private final List<PathnameDirectoryLevel> directoryLevels;

	/**
	 * The pathname directory type.
	 */
	private final PathnameDirectoryType pathnameDirectoryType;

	/**
	 * Public constructor.
	 *
	 * @param pathnameDirectoryType
	 * 		the pathname directory type (ABSOLUTE or RELATIVE)
	 */
	public PathnameDirectoryComponent(final PathnameDirectoryType pathnameDirectoryType) {
		this(pathnameDirectoryType, Collections.emptyList());
	}

	/**
	 * Public constructor.
	 *
	 * @param directoryLevels
	 * 		the pathname directory levels
	 * @param pathnameDirectoryType
	 * 		the pathname directory type (ABSOLUTE or RELATIVE)
	 */
	public PathnameDirectoryComponent(final PathnameDirectoryType pathnameDirectoryType, final List<PathnameDirectoryLevel> directoryLevels) {
		if (CollectionUtils.isNotEmpty(directoryLevels)
				&& (pathnameDirectoryType == PathnameDirectoryType.ABSOLUTE)) {

			final PathnameDirectoryLevel firstElement = directoryLevels.get(0);
			if ((firstElement.getDirectoryLevelType() == PathnameDirectoryLevelType.BACK)
					|| (firstElement.getDirectoryLevelType() == PathnameDirectoryLevelType.UP)) {
				// TODO: should this take a stream!??!?
				throw new FileErrorException(":ABSOLUTE must not be followed by :BACK or :UP.", null);
			}
		}

		this.directoryLevels = directoryLevels;
		this.pathnameDirectoryType = pathnameDirectoryType;
	}

	/**
	 * Getter for pathname directory component {@link #directoryLevels} property.
	 *
	 * @return pathname directory component {@link #directoryLevels} property
	 */
	public List<PathnameDirectoryLevel> getDirectoryLevels() {
		return directoryLevels;
	}

	/**
	 * Getter for pathname directory component {@link #pathnameDirectoryType} property.
	 *
	 * @return pathname directory component {@link #pathnameDirectoryType} property
	 */
	public PathnameDirectoryType getPathnameDirectoryType() {
		return pathnameDirectoryType;
	}
}
