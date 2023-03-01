/*
 * This file is part of the PDF Split And Merge source code
 * Created on 02/nov/2013
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
package org.pdfsam.service.tool;

import jakarta.inject.Inject;
import jakarta.inject.Named;
import org.pdfsam.persistence.DefaultEntityRepository;
import org.pdfsam.persistence.PersistenceException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Andrea Vacondio
 */
public class DefaultUsageService implements UsageService {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultUsageService.class);
    static final String TOOL_USAGE_KEY = "module.usage";
    static final String TASKS_EXECUTED_KEY = "tasks.executed";
    private final DefaultEntityRepository<ToolUsage> repo;

    @Inject
    DefaultUsageService(@Named("usageRepository") DefaultEntityRepository<ToolUsage> repo) {
        this.repo = repo;
    }

    @Override
    public void incrementUsageFor(String moduleId) {
        try {
            var usage = repo.get(TOOL_USAGE_KEY, new ToolUsage(moduleId)).inc();
            repo.save(TOOL_USAGE_KEY, usage);
            LOG.trace("Usage incremented for tool {}", moduleId);
        } catch (PersistenceException e) {
            LOG.error("Unable to increment tools usage statistics", e);
        } finally {
            incrementTotalUsage();
        }
    }

    @Override
    public void clear() {
        try {
            this.repo.clean();
        } catch (PersistenceException e) {
            LOG.error("Unable to clear latest news store", e);
        }
    }

    @Override
    public long getTotalUsages() {
        return repo.getLong(TASKS_EXECUTED_KEY, 0);
    }

    private void incrementTotalUsage() {
        repo.saveLong(TASKS_EXECUTED_KEY, repo.getLong(TASKS_EXECUTED_KEY, 0) + 1);
    }
}
