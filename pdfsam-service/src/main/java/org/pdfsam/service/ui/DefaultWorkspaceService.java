/*
 * This file is part of the PDF Split And Merge source code
 * Created on 10/dic/2014
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
package org.pdfsam.service.ui;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.inject.Inject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.Map;

import static java.util.Optional.ofNullable;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * Implementation of the workspace service where data is stored and loaded in json format
 *
 * @author Andrea Vacondio
 */
public class DefaultWorkspaceService implements WorkspaceService {
    private static final Logger LOG = LoggerFactory.getLogger(DefaultWorkspaceService.class);
    private final ObjectMapper objectMapper;

    @Inject
    public DefaultWorkspaceService(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

    @Override
    public void saveWorkspace(Map<String, Map<String, String>> data, File destination) {
        requireNotNullArg(destination, "Destination file cannot be null");
        LOG.debug(i18n().tr("Saving workspace data to {0}", destination.getAbsolutePath()));
        try {
            writeToFile(data, destination);
            LOG.info(i18n().tr("Workspace saved"));
        } catch (Exception e) {
            // make it unchecked
            throw new RuntimeException(e);
        }
    }

    @Override
    public void writeToFile(Map<String, Map<String, String>> data, File destination) throws IOException {
        objectMapper.writeValue(destination, data);
    }

    @Override
    @SuppressWarnings("unchecked")
    public Map<String, Map<String, String>> loadWorkspace(File workspace) {
        requireNotNullArg(workspace, "Workspace file cannot be null");
        try {
            return ofNullable(objectMapper.readValue(workspace, Map.class)).orElseGet(Collections::emptyMap);
        } catch (Exception e) {
            // make it unchecked
            throw new RuntimeException(e);
        }
    }

}
