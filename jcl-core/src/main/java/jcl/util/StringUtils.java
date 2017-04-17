package jcl.util;

import lombok.experimental.UtilityClass;

@UtilityClass
public class StringUtils {

	/**
	 * Adapted from {@link org.apache.commons.lang3.StringUtils#indexOfDifference(CharSequence, CharSequence)} to
	 * determine the index where the two provided {@link CharSequence} objects are the same.
	 *
	 * @param cs1
	 * 		the first {@link CharSequence}
	 * @param cs2
	 * 		the second {@link CharSequence}
	 *
	 * @return the index where cs1 and cs2 begin to differ; -1 if they are equal
	 */
	public static int indexOfSameness(final CharSequence cs1, final CharSequence cs2) {
		if (cs1 == cs2) {
			return 0;
		}
		if ((cs1 == null) || (cs2 == null)) {
			return org.apache.commons.lang3.StringUtils.INDEX_NOT_FOUND;
		}
		int i;
		for (i = 0; (i < cs1.length()) && (i < cs2.length()); ++i) {
			if (cs1.charAt(i) == cs2.charAt(i)) {
				break;
			}
		}
		if ((i < cs2.length()) || (i < cs1.length())) {
			return i;
		}
		return org.apache.commons.lang3.StringUtils.INDEX_NOT_FOUND;
	}
}
