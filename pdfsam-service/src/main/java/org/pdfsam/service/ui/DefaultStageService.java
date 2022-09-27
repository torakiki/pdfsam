/*
 * This file is part of the PDF Split And Merge source code
 * Created on 07/ott/2014
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
package org.pdfsam.service.ui;

import jakarta.inject.Inject;
import jakarta.inject.Named;
import org.pdfsam.model.ui.StageStatus;
import org.pdfsam.persistence.DefaultEntityRepository;
import org.pdfsam.persistence.PersistenceException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Default implementation of a {@link StageService}
 *
 * @author Andrea Vacondio
 */
class DefaultStageService implements StageService {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultStageService.class);
    static final String STAGE_STATUS_KEY = "stage.status";
    private final DefaultEntityRepository<StageStatus> repo;

    @Inject
    DefaultStageService(@Named("stageStatusRepository") DefaultEntityRepository<StageStatus> repo) {
        this.repo = repo;
    }

    @Override
    public void save(StageStatus status) {
        try {
            repo.save(STAGE_STATUS_KEY, status);
            LOG.trace("Stage status saved {}", status);
        } catch (PersistenceException e) {
            LOG.error("Unable to save Stage status", e);
        }
    }

    @Override
    public StageStatus getLatestStatus() {
        try {
            return this.repo.get(STAGE_STATUS_KEY, StageStatus.NULL);
        } catch (PersistenceException e) {
            LOG.error("Unable to get latest stage status", e);
        }
        return StageStatus.NULL;
    }

    @Override
    public void clear() {
        try {
            this.repo.clean();
        } catch (PersistenceException e) {
            LOG.error("Unable to clear stage status", e);
        }
    }
}
