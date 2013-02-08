/*
 * Created on 08/feb/2013
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.configuration;

import java.io.IOException;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.apache.commons.lang3.StringUtils.EMPTY;

/**
 * Various PDFsam properties like versions, ...
 * 
 * @author Andrea Vacondio
 * 
 */
public final class PdfsamProperties {

    private static final Logger LOG = LoggerFactory.getLogger(PdfsamConfig.class);

    public static final String VERSION = PdfsamPropertiesLoaderHolder.LOADER.getVersion();
    public static final String PACKAGE = PdfsamPropertiesLoaderHolder.LOADER.getPackageType();
    public static final String BUILD_DATE = PdfsamPropertiesLoaderHolder.LOADER.getBuildDate();

    private PdfsamProperties() {
        // hide
    }

    /**
     * Loader for the pdfsam properties.
     * 
     * @author Andrea Vacondio
     * 
     */
    private static final class PdfsamPropertiesLoader {

        private static final String PDFSAM_PROPERTIES = "/pdfsam.properties";
        private static final String VERSION_PROPERTY = "pdfsam.version";
        private static final String PACKAGE_PROPERTY = "pdfsam.package";
        private static final String BUILDDATE_PROPERTY = "pdfsam.builddate";

        private String version = EMPTY;
        private String packageType = EMPTY;
        private String buildDate = EMPTY;

        private PdfsamPropertiesLoader() {
            Properties props = new Properties();
            try {
                LOG.trace("Loading properties from {}", PDFSAM_PROPERTIES);
                props.load(PdfsamPropertiesLoader.class.getResourceAsStream(PDFSAM_PROPERTIES));
            } catch (IOException e) {
                LOG.warn("Unable to load pdfsam properties.", e);
            }
            version = props.getProperty(VERSION_PROPERTY, "UNKNOWN");
            packageType = props.getProperty(PACKAGE_PROPERTY, "UNKNOWN");
            buildDate = props.getProperty(BUILDDATE_PROPERTY, "UNKNOWN");
        }

        String getVersion() {
            return version;
        }

        String getPackageType() {
            return packageType;
        }

        String getBuildDate() {
            return buildDate;
        }

    }

    /**
     * lazy singleton initialization holder.
     * 
     * @author Andrea Vacondio
     * 
     */
    private static final class PdfsamPropertiesLoaderHolder {

        private PdfsamPropertiesLoaderHolder() {
            // hide constructor
        }

        static final PdfsamPropertiesLoader LOADER = new PdfsamPropertiesLoader();
    }
}
