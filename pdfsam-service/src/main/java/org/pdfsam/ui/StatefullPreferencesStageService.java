/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 07/ott/2014
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
package org.pdfsam.ui;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

import java.io.IOException;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import javax.annotation.PreDestroy;
import javax.inject.Named;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.jr.ob.JSON;

/**
 * Statefull implementation of a {@link StageService} using {@link Preferences}.
 * 
 * @author Andrea Vacondio
 *
 */
@Named
class StatefullPreferencesStageService implements StageService {

    private static final Logger LOG = LoggerFactory.getLogger(StatefullPreferencesStageService.class);
    static final String STAGE_PATH = "/org/pdfsam/stage";
    static final String STAGE_STATUS_KEY = "stage.status";

    @PreDestroy
    void flush() {
        Preferences prefs = Preferences.userRoot().node(STAGE_PATH);
        try {
            LOG.trace("Flushing stage information");
            prefs.flush();
        } catch (BackingStoreException e) {
            LOG.error("Unable to stage status information", e);
        }
    }

    public void save(StageStatus status) {
        Preferences node = Preferences.userRoot().node(STAGE_PATH);
        try {
            node.put(STAGE_STATUS_KEY, JSON.std.asString(status));
            LOG.trace("Stage status saved {}", status);
        } catch (IOException e) {
            LOG.error("Unable to increment modules usage statistics", e);
        }
    }

    public StageStatus getLatestStatus() {
        Preferences node = Preferences.userRoot().node(STAGE_PATH);
        try {
            String statusString = node.get(STAGE_STATUS_KEY, "");
            if (isNotBlank(statusString)) {
                return JSON.std.beanFrom(StageStatus.class, statusString);
            }
        } catch (IOException e) {
            LOG.error("Unable to get latest stage status", e);
        }
        return StageStatus.NULL;
    }

    void clearStageStatus() {
        Preferences prefs = Preferences.userRoot().node(STAGE_PATH);
        try {
            prefs.removeNode();
            prefs.flush();
        } catch (BackingStoreException e) {
            LOG.error("Unable to clear stage status", e);
        }
    }
}
