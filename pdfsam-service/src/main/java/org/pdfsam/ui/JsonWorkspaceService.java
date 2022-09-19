/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/dic/2014
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
package org.pdfsam.ui;

import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

import java.io.File;
import java.io.FileInputStream;
import java.util.Collections;
import java.util.Map;

import org.pdfsam.i18n.I18nContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.jr.ob.JSON;
import com.fasterxml.jackson.jr.ob.JSON.Feature;

/**
 * Implementation of the workspace service where data is stored and loaded in json format
 * 
 * @author Andrea Vacondio
 *
 */
class JsonWorkspaceService implements WorkspaceService {
    private static final Logger LOG = LoggerFactory.getLogger(JsonWorkspaceService.class);
    private JSON jackson = new JSON().without(Feature.USE_FIELDS).with(JSON.Feature.PRETTY_PRINT_OUTPUT)
            .without(JSON.Feature.WRITE_NULL_PROPERTIES);

    @Override
    public void saveWorkspace(Map<String, Map<String, String>> data, File destination) {
        requireNotNullArg(destination, "Destination file cannot be null");
        LOG.debug(i18n().tr("Saving workspace data to {0}", destination.getAbsolutePath()));
        try {
            jackson.write(data, destination);
            LOG.info(i18n().tr("Workspace saved"));
        } catch (Exception e) {
            // make it unchecked
            throw new RuntimeException(e);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public Map<String, Map<String, String>> loadWorkspace(File workspace) {
        requireNotNullArg(workspace, "Workspace file cannot be null");
        Map<String, Map<String, String>> data = Collections.emptyMap();
        try (FileInputStream stream = new FileInputStream(workspace)) {
            data = (Map) jackson.mapFrom(stream);
        } catch (Exception e) {
            // make it unchecked
            throw new RuntimeException(e);
        }
        return data;
    }

}
