package jcl.structs.pathnames;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameHost} is the object representation of the 'host' element of a Lisp 'pathname' type.
 */
public final class PathnameHost {

	private final String host;
	private final PathnameComponentType componentType;

	/**
	 * Public constructor.
	 */
	public PathnameHost() {
		host = null;
		componentType = PathnameComponentType.UNSPECIFIC;
	}

	/**
	 * Public constructor.
	 *
	 * @param host
	 * 		the pathname host
	 */
	public PathnameHost(final String host) {
		this.host = host;

		if (StringUtils.isEmpty(host)) {
			componentType = PathnameComponentType.NIL;
		} else {
			componentType = null;
		}
	}

	/**
	 * Public constructor.
	 *
	 * @param componentType
	 * 		pathname host component type
	 */
	public PathnameHost(final PathnameComponentType componentType) {
		host = null;
		this.componentType = componentType;
	}

	/**
	 * Getter for pathname host value.
	 *
	 * @return pathname host value
	 */
	public String getHost() {
		return host;
	}

	/**
	 * Getter for pathname host component type.
	 *
	 * @return pathname host component type
	 */
	public PathnameComponentType getComponentType() {
		return componentType;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
