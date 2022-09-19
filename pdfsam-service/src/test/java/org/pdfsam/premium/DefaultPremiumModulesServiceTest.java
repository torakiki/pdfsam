/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26 ott 2015
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import org.apache.commons.io.FileUtils;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.pdfsam.AppBrand;
import org.pdfsam.BrandableProperty;
import org.pdfsam.model.premium.PremiumModule;
import org.pdfsam.model.premium.PremiumProduct;

import java.io.File;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * @author Andrea Vacondio
 *
 */
public class DefaultPremiumModulesServiceTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private DefaultPremiumModulesService victim;
    private AppBrand appBrand;

    @Before
    public void setUp() {
        appBrand = mock(AppBrand.class);
        victim = new DefaultPremiumModulesService(appBrand);
    }

    @Test
    public void testGetLatestNews() throws Exception {
        File file = folder.newFile();
        FileUtils.copyInputStreamToFile(getClass().getResourceAsStream("/test_premium_modules.json"), file);
        when(appBrand.property(BrandableProperty.PREMIUM_TOOLS_URL)).thenReturn(file.toURI().toString());
        List<PremiumModule> modules = victim.getPremiumModules();
        assertEquals(1, modules.size());
        assertEquals("module-name", modules.get(0).getName());
        assertEquals("module-description", modules.get(0).getDescription());
        assertEquals(1, modules.get(0).getId());
        assertEquals(PremiumProduct.VISUAL, modules.get(0).getProduct());
        assertEquals("http://www.pdfsam.org/", modules.get(0).getUrl());
    }

}
