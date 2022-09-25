/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13/ago/2014
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
package org.pdfsam.service.tool;

import org.pdfsam.model.tool.Tool;

import static org.sejda.commons.util.RequireUtils.requireNotBlank;

/**
 * Helper class used to bind {@link Tool}s and their usage statistics
 *
 * @author Andrea Vacondio
 */
class ModuleUsage {
    private String moduleId;
    private long lastSeen = 0;
    private long totalUsed = 0;

    public ModuleUsage() {

    }

    public ModuleUsage(String moduleId) {
        requireNotBlank(moduleId, "ModuleId cannot be blank");
        this.moduleId = moduleId;
    }

    public String getModuleId() {
        return moduleId;
    }

    public void setModuleId(String moduleId) {
        this.moduleId = moduleId;
    }

    public long getLastSeen() {
        return lastSeen;
    }

    public void setLastSeen(long lastSeen) {
        this.lastSeen = lastSeen;
    }

    public long getTotalUsed() {
        return totalUsed;
    }

    public void setTotalUsed(long totalUsed) {
        this.totalUsed = totalUsed;
    }

    ModuleUsage inc() {
        this.totalUsed++;
        setLastSeen(System.currentTimeMillis());
        return this;
    }

}
