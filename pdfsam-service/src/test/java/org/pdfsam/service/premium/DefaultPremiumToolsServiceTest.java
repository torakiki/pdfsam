/*
 * This file is part of the PDF Split And Merge source code
 * Created on 26 ott 2015
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.service.premium;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.core.AppBrand;
import org.pdfsam.core.BrandableProperty;
import org.pdfsam.model.premium.PremiumProduct;
import org.pdfsam.model.premium.PremiumTool;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * @author Andrea Vacondio
 */
public class DefaultPremiumToolsServiceTest {

    private DefaultPremiumToolsService victim;
    private AppBrand appBrand;

    @BeforeEach
    public void setUp() {
        appBrand = mock(AppBrand.class);
        var mapper = JsonMapper.builder().addModule(new Jdk8Module()).addModule(new JavaTimeModule())
                .enable(DeserializationFeature.FAIL_ON_NUMBERS_FOR_ENUMS).enable(SerializationFeature.INDENT_OUTPUT)
                .disable(SerializationFeature.WRITE_DATE_TIMESTAMPS_AS_NANOSECONDS)
                .visibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY)
                .serializationInclusion(JsonInclude.Include.NON_EMPTY)
                .build();
        victim = new DefaultPremiumToolsService(appBrand, mapper);
    }

    @Test
    public void testGetLatestNews(@TempDir Path folder) throws Exception {
        var file = Files.createTempFile(folder, null, null);
        Files.copy(getClass().getResourceAsStream("/test_premium_modules.json"), file,
                StandardCopyOption.REPLACE_EXISTING);
        when(appBrand.property(BrandableProperty.PREMIUM_TOOLS_URL)).thenReturn(file.toFile().toURI().toString());
        List<PremiumTool> tools = victim.getPremiumTools();
        assertEquals(1, tools.size());
        assertEquals("module-name", tools.get(0).name());
        assertEquals("module-description", tools.get(0).description());
        assertEquals(1, tools.get(0).id());
        assertEquals(PremiumProduct.VISUAL, tools.get(0).product());
        assertEquals("http://www.pdfsam.org/", tools.get(0).url());
    }

}
