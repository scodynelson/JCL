package jcl.pathnames;

import jcl.conditions.exceptions.FileErrorException;
import org.apache.commons.collections4.CollectionUtils;

import java.util.List;

/**
 * The {@code PathnameDirectoryComponent} is the object representation of a directory component of the 'directory'
 * element of a Lisp 'pathname' type.
 */
public final class PathnameDirectoryComponent {

	private final PathnameDirectoryType pathnameDirectoryType;
	private final List<PathnameDirectoryLevel> directoryLevels;

	/**
	 * Public constructor.
	 *
	 * @param directoryLevels       the pathname directory levels
	 * @param pathnameDirectoryType the pathname directory type (ABSOLUTE or RELATIVE)
	 */
	public PathnameDirectoryComponent(final PathnameDirectoryType pathnameDirectoryType, final List<PathnameDirectoryLevel> directoryLevels) {
		if (CollectionUtils.isNotEmpty(directoryLevels) && (pathnameDirectoryType == PathnameDirectoryType.ABSOLUTE)) {
			final PathnameDirectoryLevel firstElement = directoryLevels.get(0);
			if ((firstElement.getDirectoryLevelType() == PathnameDirectoryLevelType.BACK)
					|| (firstElement.getDirectoryLevelType() == PathnameDirectoryLevelType.UP)) {
				throw new FileErrorException(":ABSOLUTE must not be followed by :BACK or :UP.");
			}
		}

		this.pathnameDirectoryType = pathnameDirectoryType;
		this.directoryLevels = directoryLevels;
	}

	/**
	 * Getter for pathname directory type.
	 *
	 * @return pathname directory type
	 */
	public PathnameDirectoryType getPathnameDirectoryType() {
		return pathnameDirectoryType;
	}

	/**
	 * Getter for pathname directory levels.
	 *
	 * @return pathname directory levels
	 */
	public List<PathnameDirectoryLevel> getDirectoryLevels() {
		return directoryLevels;
	}

	@Override
	public String toString() {
		return "PathnameDirectory{"
				+ "directoryLevels=" + directoryLevels
				+ ", pathnameDirectoryType=" + pathnameDirectoryType
				+ '}';
	}
}
