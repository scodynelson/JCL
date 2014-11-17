package jcl.structs.pathnames;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameName} is the object representation of the 'name' element of a Lisp 'pathname' type.
 */
public final class PathnameName {

	private final String name;
	private final PathnameComponentType componentType;

	/**
	 * Public constructor.
	 */
	public PathnameName() {
		name = null;
		componentType = PathnameComponentType.UNSPECIFIC;
	}

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the pathname name
	 */
	public PathnameName(final String name) {
		this.name = name;

		if (StringUtils.isEmpty(name)) {
			componentType = PathnameComponentType.NIL;
		} else if ("*".equalsIgnoreCase(name)) {
			componentType = PathnameComponentType.WILD;
		} else {
			componentType = null;
		}
	}

	/**
	 * Public constructor.
	 *
	 * @param componentType
	 * 		pathname name component type
	 */
	public PathnameName(final PathnameComponentType componentType) {
		name = null;
		this.componentType = componentType;
	}

	/**
	 * Getter for pathname name {@link #name} property.
	 *
	 * @return pathname name {@link #name} property
	 */
	public String getName() {
		return name;
	}

	/**
	 * Getter for pathname name {@link #componentType} property.
	 *
	 * @return pathname name {@link #componentType} property
	 */
	public PathnameComponentType getComponentType() {
		return componentType;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
