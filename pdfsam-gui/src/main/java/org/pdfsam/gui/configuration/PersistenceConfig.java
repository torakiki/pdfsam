package org.pdfsam.gui.configuration;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 05/10/22
 * Copyright 2022 by Sober Lemur S.r.l. (info@soberlemur.com).
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

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import jakarta.inject.Named;
import org.pdfsam.injector.Provides;
import org.pdfsam.model.ui.StageStatus;
import org.pdfsam.persistence.DefaultEntityRepository;
import org.pdfsam.persistence.PreferencesRepository;
import org.pdfsam.service.tool.ToolUsage;

/**
 * @author Andrea Vacondio
 */
public class PersistenceConfig {

    @Provides
    public ObjectMapper jsonMapper() {
        return JsonMapper.builder().addModule(new Jdk8Module()).addModule(new JavaTimeModule())
                .enable(DeserializationFeature.FAIL_ON_NUMBERS_FOR_ENUMS).enable(SerializationFeature.INDENT_OUTPUT)
                .disable(SerializationFeature.WRITE_DATE_TIMESTAMPS_AS_NANOSECONDS)
                .visibility(PropertyAccessor.FIELD, JsonAutoDetect.Visibility.ANY)
                .defaultPropertyInclusion(JsonInclude.Value.ALL_NON_EMPTY)
                .build();
    }

    @Provides
    @Named("stageStatusRepository")
    DefaultEntityRepository<StageStatus> stageStatusRepository(ObjectMapper mapper) {
        return new DefaultEntityRepository<>("/org/pdfsam/stage", mapper, StageStatus.class);
    }

    @Provides
    @Named("recentWorkspacesRepository")
    PreferencesRepository recentWorkspacesRepository() {
        return new PreferencesRepository("/org/pdfsam/user/workspaces");
    }

    @Provides
    @Named("newsRepository")
    PreferencesRepository newsRepo() {
        return new PreferencesRepository("/org/pdfsam/user/news");
    }

    @Provides
    @Named("usageRepository")
    DefaultEntityRepository<ToolUsage> usageRepository(ObjectMapper mapper) {
        return new DefaultEntityRepository<>("/org/pdfsam/modules/usage", mapper, ToolUsage.class);
    }

    @Provides
    @Named("toolsOrderRepository")
    PreferencesRepository toolsOrderRepo() {
        return new PreferencesRepository("/org/pdfsam/tools/order");
    }
}
