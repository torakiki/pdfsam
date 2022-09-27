/*
 * This file is part of the PDF Split And Merge source code
 * Created on 14/ago/2014
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
package org.pdfsam.service.update;

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
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.pdfsam.core.AppBrand;
import org.pdfsam.core.BrandableProperty;
import org.pdfsam.test.ClearEventStudioExtension;
import org.pdfsam.test.JavaFxThreadInitializeExtension;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * @author Andrea Vacondio
 */
@ExtendWith({ ClearEventStudioExtension.class, JavaFxThreadInitializeExtension.class })
public class DefaultUpdateServiceTest {

    private DefaultUpdateService victim;
    private JsonMapper mapper;
    private AppBrand appBrand;

    @BeforeEach
    public void setUp() {
        appBrand = mock(AppBrand.class);
        this.mapper = JsonMapper.builder().addModule(new Jdk8Module()).addModule(new JavaTimeModule())
                .enable(DeserializationFeature.FAIL_ON_NUMBERS_FOR_ENUMS).enable(SerializationFeature.INDENT_OUTPUT)
                .disable(SerializationFeature.WRITE_DATE_TIMESTAMPS_AS_NANOSECONDS)
                .visibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY)
                .serializationInclusion(JsonInclude.Include.NON_EMPTY)
                .build();
        victim = new DefaultUpdateService(appBrand, mapper);
    }

    @Test
    public void pasitiveCheckForUpdates(@TempDir Path folder) throws IOException {
        var file = Files.createTempFile(folder, null, null);
        Files.copy(getClass().getResourceAsStream("/test_current_version.json"), file,
                StandardCopyOption.REPLACE_EXISTING);
        when(appBrand.property(BrandableProperty.CURRENT_VERSION_URL)).thenReturn(file.toFile().toURI().toString());
        assertEquals("3.0.0", victim.getLatestVersion());
    }

    @Test
    public void negativeCheckForUpdates(@TempDir Path folder) throws IOException {
        var file = Files.createTempFile(folder, null, null);
        Files.copy(getClass().getResourceAsStream("/test_current_version_negative.json"), file,
                StandardCopyOption.REPLACE_EXISTING);
        when(appBrand.property(BrandableProperty.CURRENT_VERSION_URL)).thenReturn(file.toFile().toURI().toString());
        assertEquals("", victim.getLatestVersion());
    }
}
