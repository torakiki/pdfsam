/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22 ott 2015
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.community;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.Test;
import org.pdfsam.ConfigurableProperty;

public class PdfsamCommunityTest {
    @Test(expected = IllegalArgumentException.class)
    public void blankName() throws IOException {
        new PdfsamCommunity(" ", "something");
    }

    @Test(expected = IllegalArgumentException.class)
    public void blankShortName() throws IOException {
        new PdfsamCommunity("Something", " ");
    }

    @Test
    public void property() throws IOException {
        PdfsamCommunity pdfsam = new PdfsamCommunity("name", "short");
        assertEquals("1.0.0", pdfsam.property(ConfigurableProperty.VERSION));
        assertEquals("Chuck", pdfsam.property(ConfigurableProperty.FEED_URL, "Chuck"));
    }
}
