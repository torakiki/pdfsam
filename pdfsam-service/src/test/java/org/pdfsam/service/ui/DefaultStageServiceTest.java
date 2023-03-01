/*
 * This file is part of the PDF Split And Merge source code
 * Created on 07/ott/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
package org.pdfsam.service.ui;

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
import org.pdfsam.model.ui.StageStatus;
import org.pdfsam.persistence.DefaultEntityRepository;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Andrea Vacondio
 */
public class DefaultStageServiceTest {
    private DefaultStageService victim;

    @BeforeEach
    public void setUp() {
        var mapper = JsonMapper.builder().addModule(new Jdk8Module()).addModule(new JavaTimeModule())
                .enable(DeserializationFeature.FAIL_ON_NUMBERS_FOR_ENUMS).enable(SerializationFeature.INDENT_OUTPUT)
                .disable(SerializationFeature.WRITE_DATE_TIMESTAMPS_AS_NANOSECONDS)
                .visibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY)
                .serializationInclusion(JsonInclude.Include.NON_EMPTY)
                .build();
        this.victim = new DefaultStageService(
                new DefaultEntityRepository<>("/test/org/pdfsam/stageservice", mapper, StageStatus.class));
    }

    @AfterEach
    public void tearDown() {
        victim.clear();
    }

    @Test
    public void saveAndGetLatest() {
        StageStatus status = new StageStatus(10, 20, 100, 200);
        victim.save(status);
        assertEquals(status, victim.getLatestStatus());
    }

    @Test
    public void testClear() {
        StageStatus status = new StageStatus(10, 20, 100, 200);
        victim.save(status);
        victim.clear();
        assertEquals(StageStatus.NULL, victim.getLatestStatus());
    }
}
