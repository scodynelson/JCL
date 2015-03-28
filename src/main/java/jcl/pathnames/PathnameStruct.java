/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import jcl.classes.BuiltInClassStruct;
import jcl.types.Pathname;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameStruct} is the object representation of a Lisp 'pathname' type.
 */
public class PathnameStruct extends BuiltInClassStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -5845491980801761678L;

	/**
	 * The {@link PathnameHost} value.
	 */
	protected final PathnameHost host;

	/**
	 * The {@link PathnameDevice} value.
	 */
	protected final PathnameDevice device;

	/**
	 * The {@link PathnameDirectory} value.
	 */
	protected final PathnameDirectory directory;

	/**
	 * The {@link PathnameName} value.
	 */
	protected final PathnameName name;

	/**
	 * The {@link PathnameType} value.
	 */
	protected final PathnameType type;

	/**
	 * The {@link PathnameVersion} value.
	 */
	protected final PathnameVersion version;

	/**
	 * The internal {@link Path} representation of the pathname.
	 */
	protected final Path path;

	/**
	 * Protected constructor.
	 *
	 * @param host
	 * 		the pathname host
	 * @param device
	 * 		the pathname device
	 * @param directory
	 * 		the pathname directory
	 * @param name
	 * 		the pathname name
	 * @param type
	 * 		the pathname type
	 * @param version
	 * 		the pathname version
	 * @param path
	 * 		the {@link Path} representation of the pathname
	 */
	protected PathnameStruct(final PathnameHost host, final PathnameDevice device, final PathnameDirectory directory,
	                         final PathnameName name, final PathnameType type, final PathnameVersion version,
	                         final Path path) {
		this(Pathname.INSTANCE, host, device, directory, name, type, version, path);
	}

	/**
	 * Protected constructor.
	 *
	 * @param pathnameType
	 * 		the pathname structure type
	 * @param host
	 * 		the pathname host
	 * @param device
	 * 		the pathname device
	 * @param directory
	 * 		the pathname directory
	 * @param name
	 * 		the pathname name
	 * @param type
	 * 		the pathname type
	 * @param version
	 * 		the pathname version
	 * @param path
	 * 		the {@link Path} representation of the pathname
	 */
	protected PathnameStruct(final Pathname pathnameType,
	                         final PathnameHost host, final PathnameDevice device, final PathnameDirectory directory,
	                         final PathnameName name, final PathnameType type, final PathnameVersion version,
	                         final Path path) {
		super(pathnameType, null, null);
		this.host = host;
		this.device = device;
		this.directory = directory;
		this.name = name;
		this.type = type;
		this.version = version;
		this.path = path;
	}

	/**
	 * Getter for pathname {@link #host} property.
	 *
	 * @return pathname {@link #host} property
	 */
	public PathnameHost getPathnameHost() {
		return host;
	}

	/**
	 * Getter for pathname {@link #device} property.
	 *
	 * @return pathname {@link #device} property
	 */
	public PathnameDevice getPathnameDevice() {
		return device;
	}

	/**
	 * Getter for pathname {@link #directory} property.
	 *
	 * @return pathname {@link #directory} property
	 */
	public PathnameDirectory getPathnameDirectory() {
		return directory;
	}

	/**
	 * Getter for pathname {@link #name} property.
	 *
	 * @return pathname {@link #name} property
	 */
	public PathnameName getPathnameName() {
		return name;
	}

	/**
	 * Getter for pathname {@link #type} property.
	 *
	 * @return pathname {@link #type} property
	 */
	public PathnameType getPathnameType() {
		return type;
	}

	/**
	 * Getter for pathname {@link #version} property.
	 *
	 * @return pathname {@link #version} property
	 */
	public PathnameVersion getPathnameVersion() {
		return version;
	}

	/**
	 * Getter for pathname {@link #path} property.
	 *
	 * @return pathname {@link #path} property
	 */
	public Path getPath() {
		return path;
	}

	/**
	 * Gets a {@link Path} from the provided pathname components.
	 *
	 * @param pathnameHost
	 * 		the pathname host
	 * @param pathnameDevice
	 * 		the pathname device
	 * @param pathnameDirectory
	 * 		the pathname directory
	 * @param pathnameName
	 * 		the pathname name
	 * @param pathnameType
	 * 		the pathname type
	 * @param pathnameVersion
	 * 		the pathname version
	 *
	 * @return the {@link Path} constructed from the provided pathname components
	 */
	protected static Path getPathFromComponents(final PathnameHost pathnameHost, final PathnameDevice pathnameDevice,
	                                            final PathnameDirectory pathnameDirectory, final PathnameName pathnameName,
	                                            final PathnameType pathnameType, final PathnameVersion pathnameVersion) {

		final StringBuilder stringBuilder = new StringBuilder();

		if (pathnameHost != null) {
			final String host = pathnameHost.getHost();
			if (host != null) {
				stringBuilder.append(host);
				stringBuilder.append("::");
			}
		}

		if (pathnameDevice != null) {
			final String device = pathnameDevice.getDevice();
			if (device != null) {
				stringBuilder.append(device);
				stringBuilder.append(':');
			}
		}

		if (pathnameDirectory != null) {
			final PathnameDirectoryComponent directoryComponent = pathnameDirectory.getDirectoryComponent();
			if (directoryComponent != null) {

				final PathnameDirectoryType directoryType = directoryComponent.getPathnameDirectoryType();
				switch (directoryType) {
					case ABSOLUTE:
						stringBuilder.append(File.separatorChar);
						break;
					case RELATIVE:
						break;
				}

				final List<PathnameDirectoryLevel> directoryLevels = directoryComponent.getDirectoryLevels();
				if (directoryLevels != null) {
					for (final PathnameDirectoryLevel directoryLevel : directoryLevels) {

						final PathnameDirectoryLevelType levelType = directoryLevel.getDirectoryLevelType();
						switch (levelType) {
							case WILD:
								stringBuilder.append('*');
								break;
							case BACK:
								stringBuilder.append("..");
								break;
							case UP:
								stringBuilder.append("..");
								break;
							case NULL:
								final String level = directoryLevel.getDirectoryLevel();
								stringBuilder.append(level);
								break;
						}
						stringBuilder.append(File.separatorChar);
					}
				}
			} else {
				final PathnameComponentType componentType = pathnameDirectory.getComponentType();
				switch (componentType) {
					case UNSPECIFIC:
						break;
					case WILD:
						stringBuilder.append('*');
						break;
					case NIL:
						break;
				}
			}
		}

		if (pathnameName != null) {
			final String name = pathnameName.getName();
			if (name != null) {
				stringBuilder.append(name);
			}
		}

		if (pathnameType != null) {
			final String type = pathnameType.getType();
			if (type != null) {
				stringBuilder.append(FilenameUtils.EXTENSION_SEPARATOR);
				stringBuilder.append(type);
			}
		}

		if (pathnameVersion != null) {
			final Integer version = pathnameVersion.getVersion();
			if (version != null) {
				stringBuilder.append(FilenameUtils.EXTENSION_SEPARATOR);
				stringBuilder.append(version);
			}
		}

		final String namestring = stringBuilder.toString();
		return Paths.get(namestring);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(host)
		                            .append(device)
		                            .append(directory)
		                            .append(name)
		                            .append(type)
		                            .append(version)
		                            .append(path)
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
		final PathnameStruct rhs = (PathnameStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(host, rhs.host)
		                          .append(device, rhs.device)
		                          .append(directory, rhs.directory)
		                          .append(name, rhs.name)
		                          .append(type, rhs.type)
		                          .append(version, rhs.version)
		                          .append(path, rhs.path)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(host)
		                                                                .append(device)
		                                                                .append(directory)
		                                                                .append(name)
		                                                                .append(type)
		                                                                .append(version)
		                                                                .append(path)
		                                                                .toString();
	}
}
