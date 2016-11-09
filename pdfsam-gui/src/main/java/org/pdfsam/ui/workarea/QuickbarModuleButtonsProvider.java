/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26/ago/2014
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
package org.pdfsam.ui.workarea;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import javax.inject.Inject;

import org.pdfsam.module.Module;
import org.pdfsam.module.UsageService;

/**
 * @author Andrea Vacondio
 *
 */
class QuickbarModuleButtonsProvider {
    private static final int RECENT_MODULES = 3;
    private static final int MAX_MODULES = 8;

    private UsageService service;
    private List<Module> modules;

    @Inject
    QuickbarModuleButtonsProvider(UsageService service, List<Module> modules) {
        this.service = service;
        this.modules = new ArrayList<>(modules);
        this.modules.sort((a, b) -> Integer.compare(a.descriptor().getPriority(), b.descriptor().getPriority()));
    }

    public List<ModuleButton> buttons() {
        Set<Module> collected = new LinkedHashSet<>();
        fillWithMostRecentlyUsed(collected);
        fillWithMostUsed(collected);
        fillWithPrioritized(collected);
        return collected.stream().map(ModuleButton::new).collect(Collectors.toList());
    }

    private void fillWithMostUsed(Set<Module> collected) {
        for (Module current : service.getMostUsed()) {
            if (collected.size() >= MAX_MODULES) {
                break;
            }
            collected.add(current);
        }
    }

    private void fillWithMostRecentlyUsed(Set<Module> collected) {
        for (Module current : service.getMostRecentlyUsed()) {
            if (collected.size() >= RECENT_MODULES) {
                break;
            }
            collected.add(current);
        }
    }

    private void fillWithPrioritized(Set<Module> collected) {
        for (Module current : modules) {
            if (collected.size() >= MAX_MODULES) {
                break;
            }
            collected.add(current);
        }
    }
}
