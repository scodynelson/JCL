package jcl.structs.pathnames;

import jcl.structs.conditions.exceptions.FileErrorException;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameDirectoryLevel} is the object representation of a specific directory level of the 'directory'
 * element of a Lisp 'pathname' type.
 */
public final class PathnameDirectoryLevel {

	private final String directoryLevel;
	private final PathnameDirectoryLevelType directoryLevelType;

	/**
	 * Public constructor.
	 *
	 * @param directoryLevel
	 * 		the directory level value
	 */
	public PathnameDirectoryLevel(final String directoryLevel) {
		this(directoryLevel, PathnameDirectoryLevelType.NULL);
	}

	/**
	 * Public constructor.
	 *
	 * @param directoryLevel
	 * 		the directory level value
	 * @param directoryLevelType
	 * 		the directory level type (WILD, BACK, or UP)
	 */
	public PathnameDirectoryLevel(final String directoryLevel, final PathnameDirectoryLevelType directoryLevelType) {
		if (StringUtils.isEmpty(directoryLevel)) {
			throw new FileErrorException("Directory level value cannot be null or empty.");
		}

		this.directoryLevel = directoryLevel;
		this.directoryLevelType = directoryLevelType;
	}

	/**
	 * Getter for pathname directory level {@link #directoryLevel} property.
	 *
	 * @return pathname directory level {@link #directoryLevel} property
	 */
	public String getDirectoryLevel() {
		return directoryLevel;
	}

	/**
	 * Getter for pathname directory level {@link #directoryLevelType} property.
	 *
	 * @return pathname directory level {@link #directoryLevelType} property
	 */
	public PathnameDirectoryLevelType getDirectoryLevelType() {
		return directoryLevelType;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
