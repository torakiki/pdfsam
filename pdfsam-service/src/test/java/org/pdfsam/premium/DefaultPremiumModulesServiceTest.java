/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26 ott 2015
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.premium;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.ConfigurableProperty;
import org.pdfsam.Pdfsam;

/**
 * @author Andrea Vacondio
 *
 */
public class DefaultPremiumModulesServiceTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private DefaultPremiumModulesService victim;
    private Pdfsam pdfsam;

    @Before
    public void setUp() {
        pdfsam = mock(Pdfsam.class);
        victim = new DefaultPremiumModulesService(pdfsam);
    }

    @Test
    public void testGetLatestNews() throws Exception {
        File file = folder.newFile();
        FileUtils.copyInputStreamToFile(getClass().getResourceAsStream("/test_premium_modules.json"), file);
        when(pdfsam.property(ConfigurableProperty.PREMIUM_MODULES_URL)).thenReturn(file.toURI().toString());
        List<PremiumModule> modules = victim.getPremiumModules();
        assertEquals(1, modules.size());
        assertEquals("module-name", modules.get(0).getName());
        assertEquals("module-description", modules.get(0).getDescription());
        assertEquals(1, modules.get(0).getId());
        assertEquals(PremiumProduct.VISUAL, modules.get(0).getProduct());
        assertEquals("http://www.pdfsam.org/", modules.get(0).getUrl());
    }

}
