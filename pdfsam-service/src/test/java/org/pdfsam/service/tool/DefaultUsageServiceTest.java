/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13/ago/2014
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
package org.pdfsam.service.tool;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.pdfsam.persistence.DefaultEntityRepository;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Andrea Vacondio
 */
public class DefaultUsageServiceTest {

    private DefaultUsageService victim;

    @BeforeEach
    public void setUp() {
        var mapper = JsonMapper.builder().addModule(new Jdk8Module()).addModule(new JavaTimeModule())
                .enable(DeserializationFeature.FAIL_ON_NUMBERS_FOR_ENUMS).enable(SerializationFeature.INDENT_OUTPUT)
                .disable(SerializationFeature.WRITE_DATE_TIMESTAMPS_AS_NANOSECONDS)
                .visibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY)
                .serializationInclusion(JsonInclude.Include.NON_EMPTY)
                .build();
        this.victim = new DefaultUsageService(
                new DefaultEntityRepository<>("/test/org/pdfsam/moduleusage", mapper, ToolUsage.class));
    }

    @AfterEach
    public void tearDown() {
        victim.clear();
    }

    @Test
    public void incrementUsageFor() {
        assertEquals(0, victim.getTotalUsages());
        victim.incrementUsageFor("moduleId");
        assertEquals(1, victim.getTotalUsages());
        victim.incrementUsageFor("another");
        assertEquals(2, victim.getTotalUsages());
        victim.incrementUsageFor("moduleId");
        assertEquals(3, victim.getTotalUsages());
    }

    @Test
    public void testClear() {
        victim.incrementUsageFor("moduleId");
        assertEquals(1, victim.getTotalUsages());
        victim.clear();
        assertEquals(0, victim.getTotalUsages());
    }

}
