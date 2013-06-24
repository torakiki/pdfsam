/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/dic/2011
 * Copyright 2011 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.configuration;

import junit.framework.Assert;

import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import org.pdfsam.configuration.PdfsamProperties;

/**
 * @author Andrea Vacondio
 * 
 */
public class PdfsamPropertiesTest {

    @Test
    public void testConstants() {
        Assert.assertTrue(StringUtils.isNotBlank(PdfsamProperties.BUILD_DATE));
        Assert.assertTrue(StringUtils.isNotBlank(PdfsamProperties.PACKAGE));
        Assert.assertTrue(StringUtils.isNotBlank(PdfsamProperties.VERSION));
    }
}
