package jcl.pathnames;

import jcl.conditions.exceptions.FileErrorException;
import org.apache.commons.lang3.StringUtils;

/**
 * The {@code PathnameDirectoryLevel} is the object representation of a specific directory level of the 'directory'
 * element of a Lisp 'pathname' type.
 */
public final class PathnameDirectoryLevel {

	private final String directoryLevel;
	private PathnameDirectoryLevelType directoryLevelType;

	/**
	 * Public constructor.
	 *
	 * @param directoryLevel the directory level value
	 */
	public PathnameDirectoryLevel(final String directoryLevel) {
		if (StringUtils.isEmpty(directoryLevel)) {
			throw new FileErrorException("Directory level value cannot be null or empty.");
		}

		this.directoryLevel = directoryLevel;
		directoryLevelType = null;
	}

	/**
	 * Public constructor.
	 *
	 * @param directoryLevelType the directory level type (WILD, BACK, or UP)
	 */
	public PathnameDirectoryLevel(final PathnameDirectoryLevelType directoryLevelType) {
		directoryLevel = null;
		this.directoryLevelType = directoryLevelType;
	}

	/**
	 * Getter for pathname directory level value.
	 *
	 * @return pathname directory level value
	 */
	public String getDirectoryLevel() {
		return directoryLevel;
	}

	/**
	 * Getter for pathname directory level type.
	 *
	 * @return pathname directory level type
	 */
	public PathnameDirectoryLevelType getDirectoryLevelType() {
		return directoryLevelType;
	}

	/**
	 * Setter for pathname directory level type.
	 *
	 * @param directoryLevelType new directoryLevelType value
	 */
	public void setDirectoryLevelType(final PathnameDirectoryLevelType directoryLevelType) {
		this.directoryLevelType = directoryLevelType;
	}

	@Override
	public String toString() {
		return "PathnameDirectoryLevel{"
				+ "directoryLevel='" + directoryLevel + '\''
				+ ", directoryLevelType=" + directoryLevelType
				+ '}';
	}
}
